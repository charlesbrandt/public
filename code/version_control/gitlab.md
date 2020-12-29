# Gitlab

Gitlab offers more than just git repository hosting.

https://gitlab.com/users/sign_in


## Add SSH keys

https://gitlab.com/profile/keys

Paste your public SSH key, which is usually contained in the file '~/.ssh/id_ed25519.pub' or '~/.ssh/id_rsa.pub' and begins with 'ssh-ed25519' or 'ssh-rsa'. Don't use your private SSH key.


## Create a new project

Use `+` sign on nav bar.


## Mirror existing projects

Have to decide on push or pull. 

### Push

https://docs.gitlab.com/ee/user/project/repository/repository_mirroring.html

    Log in to Gitlab

    Navigate to your project’s Settings > Repository and expand the Mirroring repositories section.
    Enter a repository URL.
    Select Push from the Mirror direction dropdown.
    Select an authentication method from the Authentication method dropdown, if necessary.
    Check the Only mirror protected branches box, if necessary.
    Check the Keep divergent refs box, if desired.
    Click the Mirror repository button to save the configuration. 

e.g.
https://git@github.com:charlesbrandt/public.git
didn't like
ssh://git@github.com:charlesbrandt/public.git

### Pull

For initial pulls / importing repositories, do that during "Create a new project" steps. (https://gitlab.com/projects/new#import_project)

To pull in changes from github, generate a token, then add it to gitlab for import from github. 

https://github.com/settings/tokens

https://gitlab.com/import/github/status






## Identity

TODO:
way to have a private email address associated with gitlab?
