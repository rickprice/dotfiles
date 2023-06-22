function aws-sso-auth
    pushd .
    cd ~/TheHomeRepot
    bazel run --config=quiet //extras/aws-sso-auth -- "$argv"
    popd
end
