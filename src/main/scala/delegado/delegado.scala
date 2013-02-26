package delegado

import language.experimental.macros
import scala.reflect.macros.Context

import annotation.target.getter

@getter
class DelegateAnnotation extends scala.annotation.StaticAnnotation

object Delegate {

  class DelegateBuilder[F,T] { this: F => type Self = F }

  def apply[T]( implicit builder: DelegateBuilder[_, T] ): builder.Self = builder.asInstanceOf[builder.Self]

  def builder[T] = macro builder_impl[T]
  
  def builder_impl[T: c.WeakTypeTag]( c: Context ) = {
    import c.universe._
    import Flag._
 
    val tpe = implicitly[c.WeakTypeTag[T]].tpe // in RC1 this will become c.absTypeOf[T]
    val clazz = tpe.typeSymbol.asClass
    
    val annotationClass = c.mirror.staticClass("delegado.DelegateAnnotation")    
    val getters = tpe.members.collect{ 
      case x: MethodSymbol if x.isGetter && x.annotations.exists(_.tpe.typeSymbol == annotationClass) => x 
      case x: ModuleSymbol if x.annotations.exists(_.tpe.typeSymbol == annotationClass) => x 
    }
    val localMembers = tpe.members.filter(_.owner == tpe.typeSymbol)

    // some of the missing parts of the reflection API
    // we should be able to come up with something better in future point releases
    val PRIVATE = (1L << 2).asInstanceOf[FlagSet]
    val PARAM = (1L << 13).asInstanceOf[FlagSet]
    val LOCAL = (1L << 19).asInstanceOf[FlagSet]
    val PARAMACCESSOR = (1L << 29).asInstanceOf[FlagSet]

    val superCtors = localMembers.collect{ case m: MethodSymbol if m.isConstructor => m }
    var fields = List.empty[ValDef]
    val ctors = superCtors.map{ superCtor =>
      val mods = if ( superCtor.isPrimaryConstructor ) Modifiers(PARAMACCESSOR|PARAM) else NoMods
      val ctorParams = superCtor.paramss.map(_.map{ ctorParam => 
        val param = ValDef(mods, newTermName(ctorParam.name.encoded), TypeTree(ctorParam.typeSignature), EmptyTree)
        if ( superCtor.isPrimaryConstructor ) fields :+= ValDef(Modifiers(PRIVATE|PARAMACCESSOR|LOCAL), newTermName(ctorParam.name.encoded), TypeTree(ctorParam.typeSignature), EmptyTree)
        param
      })
      val ctorBodyInitExpr = Select(Super(This(newTypeName("")), newTypeName("")), nme.CONSTRUCTOR)
      val ctorBody = ctorParams.foldLeft( ctorBodyInitExpr: Tree ){ case (cur, argList) =>  Apply(cur, argList.map{ arg => Ident( arg.name ) } ) }
      DefDef(NoMods, nme.CONSTRUCTOR, /*FIXME*/Nil, ctorParams, TypeTree(), Block(List(ctorBody), Literal(Constant(()))))
    }.toList
    val forwarders = getters.flatMap{ getter =>
      val retType = getter match { 
        case m: MethodSymbol => m.returnType
        case m: ModuleSymbol => m.typeSignature
      }
      def isExplicitlyOverriden(m: MethodSymbol) = {
        localMembers.exists{m2 => m2.name == m.name && m2.typeSignature =:= m.typeSignature}
      }
      def isDeclared(m: MethodSymbol) = {
        tpe.members.exists{m2 => m2.name == m.name && m2.typeSignature =:= m.typeSignature}
      }
      val forwardedMethods = retType.members.collect { case x: MethodSymbol if x.isPublic && !x.isConstructor  && !x.isFinal && !isExplicitlyOverriden(x) => x }
      forwardedMethods.map{ m =>
        val args = m.paramss.map(_.map{ param => ValDef(/* FIXME */NoMods, param.name.asInstanceOf[TermName], TypeTree(param.typeSignature), EmptyTree)})
        val initExpr = Select(Select(This(newTypeName("")), getter.name), m.name)
        val expr = args.foldLeft( initExpr: Tree ){ case (cur, argList) =>  Apply(cur, argList.map{ arg => Ident( arg.name ) })}
        val mods = if ( isDeclared( m ) ) Modifiers(OVERRIDE) else NoMods /* TODO: pass along the original modifiers of the forwarded method */
        DefDef(mods, m.name, /* FIXME */Nil, args, TypeTree(), Block(Nil, expr))
      }
    }.toList

    val tmpl = Template(List(Ident(clazz)), emptyValDef, fields ++ ctors ++ forwarders)
    val cdef = ClassDef(NoMods, newTypeName(c.fresh(clazz.name + "$delegate")), Nil, tmpl)

    val factoryMethods = ctors.flatMap{ ctor =>
      val argValues = ctor.vparamss.map(_.map{ arg => Ident(arg.name) } )
      val instantiation = New(Ident(cdef.name), argValues )
      val argDefs = ctor.vparamss.map(_.map{ arg => ValDef(Modifiers(PARAM), arg.name, arg.tpt, EmptyTree) } )
      val facMth = DefDef( NoMods, newTermName("apply"), /*FIXME*/Nil, argDefs, TypeTree(), instantiation )
      // Same as facMth but renamed as "build" which is nicer than "apply" when we need to explicitly specify the method name
      val callToFacMth = ctor.vparamss.foldLeft( Select(This(newTypeName("")), facMth.name): Tree ){ case (cur, argList) =>  Apply(cur, argList.map{ arg => Ident( arg.name ) } ) }
      val aliasedFacMth = DefDef( facMth.mods, newTermName("build"), facMth.tparams, facMth.vparamss.map(_.map(_.duplicate)), facMth.tpt.duplicate, callToFacMth )
      List(facMth, aliasedFacMth)
    }
    val factoryCtorBody = Apply(Select(Super(This(newTypeName("")), newTypeName("")), nme.CONSTRUCTOR), Nil)
    val factoryCtor = DefDef(NoMods, nme.CONSTRUCTOR, Nil, List(Nil), TypeTree(), Block(List(factoryCtorBody), Literal(Constant(()))))
    val factoryTypeName = newTypeName(c.fresh(clazz.name + "$delegatefactory"))
    val factoryTpt = Template(List(AppliedTypeTree(Ident(c.mirror.staticClass("delegado.Delegate.DelegateBuilder")), List(Ident(factoryTypeName), Ident(tpe.typeSymbol.name)))), emptyValDef, factoryCtor +: factoryMethods)
    val factoryDef = ClassDef(NoMods, factoryTypeName, Nil, factoryTpt)

    c.Expr[Any](Block(List(cdef , factoryDef), New(Ident(factoryDef.name), Nil)))
  }
}
 
