if [ -z "$releaseVersion" ]; then
    # incremental release
    make zip
    scp dist/emacs-runtime-`git describe --tags`.zip buildfix@porthos:/u01/web/versions/emacs_runtime/emacs-runtime-dev.zip
else
    VERSION=$releaseVersion make zip
    git tag $releaseVersion -m "Emacs runtime $releaseVersion released"
    git push origin $releaseVersion
    scp dist/emacs-runtime-$releaseVersion.zip buildfix@porthos:/u01/web/versions/emacs_runtime/
    ssh buildfix@porthos "cd /u01/web/versions/emacs_runtime/ && ln -sf emacs-runtime-$releaseVersion.zip emacs-runtime-latest.zip"
fi
