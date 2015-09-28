#!/bin/bash -x
# Creates a new SVN revision and exports it to R-forge

if [ -n "$1" ];
then
  USER="$1"
else
  echo "Usage: $0 <r-forge> account"
  exit 0
fi

TMPDIR="/tmp/rforge_$RANDOM"
mkdir $TMPDIR

REMOTESVN="svn+ssh://$USER@r-forge.r-project.org/svnroot/spwarnings"

CASPR_GIT='http://github.com/fdschneider/caspr'
SPWRN_GIT='http://github.com/fdschneider/spatial_warnings'

# Check out current version
cd ${TMPDIR}
svn checkout ${REMOTESVN}
svn remove ./spwarnings/pkg/*

cd ${TMPDIR}/spwarnings/pkg

git clone ${CASPR_GIT}
git clone ${SPWRN_GIT}

svn add caspr spatial_warnings

svn commit -m "Exported from git repositories"

rm -rf "${TMPDIR}"
