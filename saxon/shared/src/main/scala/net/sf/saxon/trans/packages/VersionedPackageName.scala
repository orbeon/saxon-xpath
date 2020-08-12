package net.sf.saxon.trans.packages

/*import net.sf.saxon.style.PackageVersion*/

class VersionedPackageName(var packageName: String/*, version: PackageVersion*/) {

 /* var packageVersion: PackageVersion = version*/

  def this(packageName: String, version: String) = {
    this(packageName)
    this.packageName = packageName
    //this.packageVersion = new PackageVersion(version)
  }

  override def toString(): String =
    packageName + " (" + toString + ")"

  def equalsIgnoringSuffix(other: VersionedPackageName): Boolean =
    packageName == other.packageName

  override def equals(obj: Any): Boolean =
    obj.isInstanceOf[VersionedPackageName] &&
      packageName == obj.asInstanceOf[VersionedPackageName].packageName

  override def hashCode(): Int = packageName.hashCode

}