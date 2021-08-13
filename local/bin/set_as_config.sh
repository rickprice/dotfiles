
    #!/usr/bin/sh -f

    export AS_USER={{ AS.user }}
    export CODE_SIGNING_PASSWD={{ AS.code_signing_password }}
    export CODE_SIGNING_CERT={{ AS.code_signing_cert }}
    
