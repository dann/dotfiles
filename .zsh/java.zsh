# Maven
export M2_HOME=~/devtools/maven
export PATH="$PATH:$M2_HOME/bin"

# Ant
export ANT_HOME=~/devtools/ant
export PATH="$PATH:$ANT_HOME/bin"

# Flex SDK
export _JAVA_OPTIONS=-Duser.language=en

export PATH="$PATH:$HOME/devtools/android-sdk-mac_86/tools"

function listMavenCompletions { reply=(cli:execute cli:execute-phase archetype:generate compile clean install test test-compile deploy package cobertura:cobertura jetty:run -Dmaven.test.skip=true -DarchetypeCatalog=http://tapestry.formos.com/maven-snapshot-repository -Dtest= `if [ -d ./src ] ; then find ./src -type f | grep -v svn | sed 's?.*/\([^/]*\)\..*?-Dtest=\1?' ; fi`); }
compctl -K listMavenCompletions mvn