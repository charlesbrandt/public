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

SSH Keys are preferred these days. Will need to configure one from gitlab to github. 


To set up a mirror from GitLab to GitHub, you need to follow these steps:

    Create a GitHub personal access token with the public_repo box checked.
    https://docs.github.com/en/free-pro-team@latest/github/authenticating-to-github/creating-a-personal-access-token
    
    

In the upper-right corner of any page, click your profile photo, then click Settings.
Settings icon in the user bar

In the left sidebar, click Developer settings.
Developer settings

In the left sidebar, click Personal access tokens.
Personal access tokens

Click Generate new token. 

Keep track of the token in your password manager (or you may need to regenerated it again later and then update it in Gitlab)


    Fill in the Git repository URL field using this format: https://<your_github_username>@github.com/<your_github_group>/<your_github_project>.git.
    Fill in Password field with your GitHub personal access token.
    Click the Mirror repository button. 

The mirrored repository will be listed. For example, https://*****:*****@github.com/<your_github_group>/<your_github_project>.git.

The repository will push soon. To force a push, click the Update now () button. 

https://docs.gitlab.com/ee/user/project/repository/repository_mirroring.html#pushing-to-a-remote-repository


    Log in to Gitlab

    Navigate to your project’s Settings > Repository and expand the Mirroring repositories section.
    Enter a repository URL.
    Select Push from the Mirror direction dropdown.
    Select an authentication method from the Authentication method dropdown, if necessary.
    Check the Only mirror protected branches box, if necessary.
    Check the Keep divergent refs box, if desired.
    Click the Mirror repository button to save the configuration. 

https://docs.gitlab.com/ee/user/project/repository/repository_mirroring.html


### Pull

For initial pulls / importing repositories, do that during "Create a new project" steps. (https://gitlab.com/projects/new#import_project)

To pull in changes from github, generate a token, then add it to gitlab for import from github. 

https://github.com/settings/tokens

https://gitlab.com/import/github/status






## Identity

TODO:
way to have a private email address associated with gitlab?
