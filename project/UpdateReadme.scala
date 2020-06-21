import sbt._, Keys._
import sbtrelease.ReleasePlugin.autoImport.ReleaseStep
import sbtrelease.Git

object UpdateReadme {
  private val snapshotMark = "SNAPSHOT"

  val updateReadmeTask = { state: State =>
    val extracted = Project.extract(state)
    val v = extracted get version
    val org =  extracted get organization
    val n = extracted get name
    val baseDir = extracted get baseDirectory
    val readmeFile = baseDir / "README.md"

    val str = s"""$org" % "$n" % "$v""""
    
    val newReadme = Predef.augmentString(IO.read(readmeFile)).lines.map{ line =>
      if (v.contains(snapshotMark))
        line.replaceFirst(s"""(?<=addSbtPlugin\\().+ % .+ % [^-]+-$snapshotMark"(?=\\))""", str)
      else
        line.replaceFirst(s"""(?<=addSbtPlugin\\().+ % .+ % [^-]+(?!-$snapshotMark)"(?=\\))""", str)
    }.mkString("", "\n", "\n")
    
    IO.write(readmeFile, newReadme)

    val git = new Git(baseDir)
    git.add(readmeFile.getName) ! state.log
    git.commit(message = "Update " + readmeFile.getName, sign = false, signOff = false) ! state.log
    
    state
  }

  val updateReadme: ReleaseStep = updateReadmeTask
}
