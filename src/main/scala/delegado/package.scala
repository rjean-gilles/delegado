package object delegado {
  // This alias is required because directly naming the annotation as "delegate"
  // will break the build on Windows because there is already a .class named "Delegate"
  // (for the Delegate **object**), and Windows's file system is case insensitive.
  type delegate = DelegateAnnotation
}
