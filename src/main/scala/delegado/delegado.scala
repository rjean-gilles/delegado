package delegado

import language.experimental.macros
import scala.reflect.macros.Context

import annotation.target.getter

@getter
class DelegateAnnotation( recursive: Boolean = false ) extends scala.annotation.StaticAnnotation

object Delegate {
  trait Mapping[-S,+T] extends (S => T)
  object Mapping {
    def apply[S,T]( f: S => T ): Mapping[S, T] = new Mapping[S, T]{
      def apply( src: S ): T = f( src )
    }
  }

  class DelegateBuilder[F,T] { this: F => type Self = F }

  def apply[T]( implicit builder: DelegateBuilder[_, T] ): builder.Self = builder.asInstanceOf[builder.Self]

  def builder[T] = macro builder_impl[T]
  
  def builder_impl[T: c.WeakTypeTag]( c: Context ) = {
    import c.universe._
    import Flag._
    
    // Some of the missing parts of the reflection API
    val PRIVATE = (1L << 2).asInstanceOf[FlagSet]
    val PARAM = (1L << 13).asInstanceOf[FlagSet]
    val LOCAL = (1L << 19).asInstanceOf[FlagSet]
    val PARAMACCESSOR = (1L << 29).asInstanceOf[FlagSet]
    val DEFAULT_GETTER_STRING = "$default$"
  
/*    
    // FIXME: this is a bit sick: we create a toolbox just to evaluate the tree
    //        that we get for the recursive parameter of anootation class DelegateAnnotation.
    //        This also forces us to add "scala-compiler" as a dependency.
    //        Another simpler solution would be to limit ourself to only literal values (forbidding
    //        arbitrary constant expressions for the value of "recursive") and just pattern match 
    //        against the Literal tree.
    import tools.reflect.ToolBox
    //val toolBox = reflect.runtime.universe.runtimeMirror(c.libraryClassLoader).mkToolBox()
    val toolBox = reflect.runtime.currentMirror.mkToolBox()
    val importer0 = reflect.runtime.universe.mkImporter(c.universe)
    val importer = importer0.asInstanceOf[reflect.runtime.universe.Importer { val from: c.universe.type }]
    def evalTree( tree: Tree ) = {
      toolBox.eval( toolBox.resetAllAttrs( importer.importTree( c.resetAllAttrs( tree.duplicate ) ) ) )
    }
*/
    //def isRecursiveDelegate( ann: Annotation ) = evalTree( ann.scalaArgs.head.duplicate )
    def isRecursiveDelegate( ann: Annotation ): Boolean = ann.scalaArgs.head match {
      case Literal( Constant( value: Boolean ) ) => value
      case Select(Select(This(x), y: Name), z: Name)
        if  x.decoded == "delegado" && 
            y.decoded == "DelegateAnnotation" && 
            z.decoded.contains( DEFAULT_GETTER_STRING )
        =>
          // This is the default value of the "recursive" parameter. We know it is "false"
          false
      case _ =>           
        c.abort( c.enclosingPosition, "cannot process '@delegado.delegate' annotation: unexpected non-literal value for 'recursive' parameter" )
    }

  
    val tpe = implicitly[c.WeakTypeTag[T]].tpe 
    val clazz = tpe.typeSymbol.asClass
        
    val mappingClass = c.mirror.staticClass("delegado.Delegate.Mapping")    
    val TypeRef( mappingPreType, mappingSymbol, _ ) = typeTag[Delegate.Mapping[Any, Any]].tpe
    def instantiatedMappingType( srcType: Type, tgtType: Type ): Type = typeRef( mappingPreType, mappingSymbol, List(srcType, tgtType) )
    def mappingImplicitValue( srcType: Type, tgtType: Type ) = c.inferImplicitValue( instantiatedMappingType( srcType, tgtType ) )
    
    val annotationClass = c.mirror.staticClass("delegado.DelegateAnnotation")    
    /*val getters = tpe.members.collect{ 
      case x: MethodSymbol if x.isGetter && x.annotations.exists(_.tpe.typeSymbol == annotationClass) => x 
      case x: ModuleSymbol if x.annotations.exists(_.tpe.typeSymbol == annotationClass) => x 
    }*/    
    
    val getters: Seq[(Symbol, Annotation)] = tpe.members.flatMap{ 
      case x: MethodSymbol if x.isGetter => x.annotations.find(_.tpe.typeSymbol == annotationClass).map(x -> _)
      case x: ModuleSymbol => x.annotations.find(_.tpe.typeSymbol == annotationClass).map(x -> _)
      case _ => None
    }.toSeq
    val localMembers = tpe.members.filter(_.owner == tpe.typeSymbol)
    
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
    val forwarders = getters.flatMap{ case (getter, getterAnnotation) =>
      val isRecursive = isRecursiveDelegate( getterAnnotation )
      val forwarderRetType = getter match { 
        case m: MethodSymbol => m.returnType
        case m: ModuleSymbol => m.typeSignature
      }
      
      def wrapForwardedMethodResult( forwardedMethod: MethodSymbol, inputTree: Tree ) = {
        if ( isRecursive && forwardedMethod.returnType =:= forwarderRetType ) { // FIXME: shouldn't this be <:< ?
          val mappingValueTree = mappingImplicitValue( forwarderRetType, tpe )
          // TODO: emit error if not found (empty "mappingValueTree")
          Apply( Select( mappingValueTree, newTermName("apply") ), List( inputTree ) )
        } else {
          inputTree
        }
      }

      def isExplicitlyOverriden(m: MethodSymbol) = {
        localMembers.exists{m2 => m2.name == m.name && m2.typeSignature =:= m.typeSignature}
      }
      def isDeclared(m: MethodSymbol) = {
        tpe.members.exists{m2 => m2.name == m.name && m2.typeSignature =:= m.typeSignature}
      }
      def mustBeForwarded( m: MethodSymbol ) = {
        m.isPublic && 
        !m.isConstructor && 
        !m.isFinal && 
        !isExplicitlyOverriden(m) &&
        m.name.decoded != "getClass" // HACK FIXME
      }
      val forwardedMethods = forwarderRetType.members.collect { case x: MethodSymbol if mustBeForwarded(x) => x }
      forwardedMethods.map{ m =>
        //val args = m.paramss.map(_.map{ param => ValDef(/* FIXME */NoMods, param.name.asInstanceOf[TermName], TypeTree(param.typeSignature), EmptyTree)})
        val args = m.paramss.map(_.map{ param => 
          val argName = param.name.asInstanceOf[TermName]
          val argTypeTree = TypeTree(param.typeSignature)
          if ( isRecursive && param.typeSignature =:= forwarderRetType ) { // FIXME: shouldn't this be <:< ?
            val mappingValueTree = mappingImplicitValue( tpe, forwarderRetType )
            // TODO: emit error if not found (empty "mappingValueTree")

            ValDef(/* FIXME */NoMods, argName, TypeTree(tpe), EmptyTree) -> 
            Apply( Select( mappingValueTree, newTermName("apply") ), List( Ident( argName ) ) )
          }
          else {
            ValDef(/* FIXME */NoMods, argName, TypeTree(param.typeSignature), EmptyTree) -> 
            Ident( argName )
          }
        })
        val initExpr = Select(Select(This(newTypeName("")), getter.name), m.name)
        val unwrappedResultExpr = args.foldLeft( initExpr: Tree ){ case (cur, argList) => Apply(cur, argList.map(_._2)) }
        val resultExpr = wrapForwardedMethodResult( m, unwrappedResultExpr )
        val mods = if ( isDeclared( m ) ) Modifiers(OVERRIDE) else NoMods /* TODO: pass along the original modifiers of the forwarded method */
        DefDef(mods, m.name, /* FIXME */Nil, args.map(_.map(_._1)), TypeTree(), Block(Nil, resultExpr))
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
 
