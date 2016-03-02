credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

val hmrcRepoHost = java.lang.System.getProperty("hmrc.repo.host", "http://example.com")

resolvers ++= Seq("hmrc-snapshots" at hmrcRepoHost + "/content/repositories/hmrc-snapshots",
  "hmrc-releases" at hmrcRepoHost + "/content/repositories/hmrc-releases",
  "typesafe-releases" at hmrcRepoHost + "/content/repositories/typesafe-releases",
  Resolver.url("hmrc-sbt-plugin-releases", url("https://dl.bintray.com/hmrc/sbt-plugin-releases"))(Resolver.ivyStylePatterns))

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.8.3")

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.3.8")

addSbtPlugin("uk.gov.hmrc" % "sbt-settings" % "3.0.0")

addSbtPlugin("uk.gov.hmrc" % "sbt-auto-build" % "0.10.0")

addSbtPlugin("uk.gov.hmrc" % "sbt-git-versioning" % "0.8.0")

addSbtPlugin("uk.gov.hmrc" % "sbt-distributables" % "0.9.0")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.3.5")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.8.0")