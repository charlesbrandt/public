# SSH

SSH allows accessing a remote computer via a "secure shell". This provides a command line interface to another system that is accessible via your network. 

    ssh username@machine_address
    

It's usually best to configure public key authentication (and even disable password authentication).


    Key pair is created (typically by the user). This is typically done with ssh-keygen.
    Private key stays with the user (and only there), while the public key is sent to the server. Typically with the ssh-copy-id utility.
    Server stores the public key (and "marks" it as authorized).
    Server will now allow access to anyone who can prove they have the corresponding private key.

via:
https://www.ssh.com/ssh/public-key-authentication
