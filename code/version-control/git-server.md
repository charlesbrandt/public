# Git Servers

Many are familiar with Github. Gitlab is a good open-sourced alternative. It's also pretty easy to host your own git repositories -- all you really need is a linux server that you have ssh access too!

## Running a server

It's pretty much as simple as having an SSH server running and being able to connect over that.

https://www.linux.com/learn/how-run-your-own-git-server
How to Run Your Own Git Server | Linux.com | The source for Linux information

https://www.google.com/search?q=linux+git+server
linux git server - Google Search

### New repository on server

Create your project's repo on your git host and check that out locally

When you're not on the server (e.g. created the repo somewhere else), you need to create a blank repo on the server first:

- Log into the server machine.
- Create a bare repo using

```
ssh [user@host-server]
mkdir -p /srv/git/home-system
cd /srv/git/home-system
```

git --version 2.28 and up:

```
git init --initial-branch=main --bare
```

git --version older than 2.28

```
git init --bare
git symbolic-ref HEAD refs/heads/main
```

[via](https://stackoverflow.com/questions/42871542/how-can-i-create-a-git-repository-with-the-default-branch-name-other-than-maste)

Check out the new bare repo on the client:

```
git clone account@server:/srv/git/project
```

May get `warning: You appear to have cloned an empty repository.`
Add some content. At that point, check to see what branch you're on. If it's still set to `master` move it to `main` and push

```
git branch -a
git branch -m main
git push origin HEAD
```

#### Add server to existing repo

Note: this requires the remote repository to exist on the server first. 

If you have an existing repo and want to add a server later

- On the client machine check for existing origins

  ```
  git remote -v
  ```

  (if you need to delete an existing origin):

  ```
  git remote remove origin
  ```
  
- push your repo to the server

  ```
  git remote add origin ssh://user@server:/GitRepos/myproject.git
  ```

  followed by

  ```
  git push origin main
  ```

May also need

  ```
  git branch --set-upstream-to origin/main
  ```

Try git show-ref to see what refs you have. Is there a refs/heads/master?

You can try git push origin HEAD:master as a more local-reference-independent solution. This explicitly states that you want to push the local ref HEAD to the remote ref master (see the git-push refspec documentation).

[via](https://stackoverflow.com/questions/4181861/message-src-refspec-master-does-not-match-any-when-pushing-commits-in-git)

then checkout to any device with:

  ```
  git clone user@server:/srv/git/repo
  ```

If the repo on the server has local files checked out (not bare), when it's time to push changes up to the server, they'll be rejected.

Being able to push is the important option in this scenario.

An alternative solution could use different branches on either the remote device and/or the server. This seems more cumbersome.

https://stackoverflow.com/questions/2816369/git-push-error-remote-rejected-master-master-branch-is-currently-checked

## Template repositories

Some hosts make it easy to designate a repository as a template. 

https://duckduckgo.com/?t=ffab&q=gitlab+mark+repostory+as+a+template&ia=web
gitlab mark repostory as a template at DuckDuckGo
https://betterprogramming.pub/forget-boilerplate-use-repository-templates-74efebbee8eb
Forget Boilerplate, Use Repository Templates! | by Liliana Nuño Silva | Better Programming
https://docs.gitlab.com/ee/user/admin_area/settings/instance_template_repository.html#help-and-feedback
Instance template repository | GitLab


## Self-hosted Interfaces

[Gitlab](gitlab.md) can be run on local infrastructure


### Gogs

https://gogs.io/docs/features/custom_template  
Custom template - Gogs  
https://gogs.io/  
Gogs: A painless self-hosted Git service  

