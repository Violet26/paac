resolvers += Resolver.url("hmrc-sbt-plugin-releases", url("https://dl.bintray.com/hmrc/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"
resolvers += "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases/"

addSbtPlugin("uk.gov.hmrc" % "sbt-auto-build" % "1.6.0")

addSbtPlugin("uk.gov.hmrc" % "sbt-git-versioning" % "0.9.0")

addSbtPlugin("uk.gov.hmrc" % "sbt-distributables" % "1.0.0")

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.5.12")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.0")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.7.0")

addSbtPlugin("com.orrsella" % "sbt-sublime" % "1.1.1")

addSbtPlugin("org.brianmckenna" % "sbt-wartremover" % "0.14")

addSbtPlugin("org.ensime" % "sbt-ensime" % "1.0.0")

addSbtPlugin("com.github.fedragon" % "sbt-todolist" % "0.6")

