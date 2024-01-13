# Templates

https://opensource.guide/starting-a-project/

https://medium.com/@smrgrace/having-a-git-repo-that-is-a-template-for-new-projects-148079b7f178
Having a git repo that is a template for new projects | by Sam Grace | Mediu

See Also

public/code/version-control/git-server.md

public/code/javascript/frameworks*


## Git templates 

have found some interesting resources on this topic recently

revise
condense

Options include:

 - start with a fresh copy -- no shared version control history
    - this is a good option -- you'll make changes. It will grow into a project of it's own. Once you get it working, no sense in pulling upstream (unless...)
    
 - fork a copy
   this is a good option if the project itself is a derivative template based on web-ui-api-db (a specific flavor)
   this way changes to the base can be tracked. 
   i suspect that changes to the base will be minimal 
   variation will happen on the different specific flavors / combinations / configurations


https://unsplash.com/photos/Gg67LLGpXnI?utm_source=unsplash&utm_medium=referral&utm_content=creditShareLink

## Degit

https://www.npmjs.com/package/degit
degit - npm

https://duckduckgo.com/?t=ffab&q=npx+degit&ia=software
npx degit at DuckDuckGo


## Cookiecutter

Python tool

Be sure that path is configured in shell:

```
# Add ~/.local/ to PATH
export PATH=$HOME/.local/bin:$PATH
```

Remember to load changes with `source ~/.bash_profile` or open a new shell session.

https://cookiecutter.readthedocs.io/en/stable/installation.html#install-cookiecutter

https://github.com/cookiecutter/cookiecutter  
GitHub - cookiecutter/cookiecutter: A cross-platform command-line utility that creates projects from cookiecutters (project templates), e.g. Python package projects, C projects.  

```
pip install cookiecutter
```

Clone a template locally to make it easier to edit the included `cookiecutter.json`




https://pypi.org/project/cookiecutter/  
cookiecutter · PyPI  
https://github.com/cookiecutter/cookiecutter  
GitHub - cookiecutter/cookiecutter: A cross-platform command-line utility that creates projects from cookiecutters (project templates), e.g. Python package projects, C projects.  
https://github.com/topics/cookiecutter-template  
cookiecutter-template · GitHub Topics · GitHub  
https://github.com/sourcery-ai/python-best-practices-cookiecutter  
GitHub - sourcery-ai/python-best-practices-cookiecutter: Python best practices project cookiecutter 🍪  

https://duckduckgo.com/?q=cookiecutter+python+asks+for+username+for+%27https%3A%2F%2Fgithub.com%27&t=ffab&ia=web  
cookiecutter python asks for username for 'https://github.com' at DuckDuckGo  
have to update `cookiecutter.json` with the defaults you want to use, if you don't want to enter them manually

https://www.cookiecutter.io/article-post/cookiecutter-alternatives  
Cookiecutter  
https://mlops-guide.github.io/Structure/starting/  
Starting a New Project with Cookiecutter - MLOps Guide  

https://duckduckgo.com/?t=ffab&q=cookiecutter+python&ia=web  
cookiecutter python at DuckDuckGo  


## Boilerplates


https://github.com/topics/boilerplate
boilerplate · GitHub Topics

https://github.com/sahat/hackathon-starter
sahat/hackathon-starter: A boilerplate for Node.js web applications
https://travis-ci.org/github/sahat/hackathon-starter
sahat/hackathon-starter - Travis CI


Vitesse has some good notes about getting started from a template:

web-ui-api-db also has some notes

https://duckduckgo.com/?t=ffab&q=git+prune+subdirectory&ia=web
git prune subdirectory at DuckDuckGo
https://gist.github.com/lausdahl/c32dafef219d6025424d
How to Extract a Subdirectory as a New git Repository · GitHub
m
https://github.com/sam-at-work/cra-with-api
sam-at-work/cra-with-api
https://duckduckgo.com/?t=ffab&q=git+repo+template+boilerplate&ia=web
git repo template boilerplate at DuckDuckGo

https://github.com/topics/template
template · GitHub Topics
https://github.com/Armour/vue-typescript-admin-template
Armour/vue-typescript-admin-template: 🖖 A vue-cli 3.0 + typescript minimal admin template
https://github.com/deadlydog/Template.NewGitRepo
deadlydog/Template.NewGitRepo: A template repository to use as a starting point for new git repos with basic boilerplate things that everyt git repo should have

https://duckduckgo.com/?t=ffab&q=gitlab+project+templates&ia=web
gitlab project templates at DuckDuckGo


## Front-end

https://github.com/topics/boilerplate
boilerplate · GitHub Topics
https://github.com/sahat/hackathon-starter
sahat/hackathon-starter: A boilerplate for Node.js web applications
https://duckduckgo.com/?t=ffab&q=gitlab+project+template+with+testing&ia=web
gitlab project template with testing at DuckDuckGo
https://docs.gitlab.com/ee/development/cicd/templates.html
Development guide for GitLab CI/CD templates | GitLab
https://gitlab.com/projects/new#create_from_template
New Project · GitLab
https://gitlab.com/gitlab-org/sample-data-templates/sample-gitlab-project
GitLab.org / Sample Data Templates / sample-gitlab-project · GitLab
https://docs.gitlab.com/ee/user/project/working_with_projects.html
Working with projects | GitLab
https://about.gitlab.com/community/contribute/project-templates/
Contributing to Project Templates | GitLab
https://gitlab.com/gitlab-org/project-templates/dotnetcore
GitLab.org / project-templates / dotnetcore · GitLab
https://docs.gitlab.com/ee/integration/gitpod.html
Gitpod Integration | GitLab
https://www.gitpod.io/features/
Features
https://duckduckgo.com/?t=ffab&q=example+git+repo+with+license+and+contributing+guide&ia=web
example git repo with license and contributing guide at DuckDuckGo
https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/creating-a-repository-on-github/licensing-a-repository
Licensing a repository - GitHub Docs
https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/creating-a-repository-on-github
Creating a repository on GitHub - GitHub Docs
https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/creating-a-repository-on-github/creating-a-template-repository
Creating a template repository - GitHub Docs
https://duckduckgo.com/?t=ffab&q=guide+to+open+source&ia=web
guide to open source at DuckDuckGo
https://opensource.guide/
Open Source Guides | Learn how to launch and grow your project.
https://opensource.guide/starting-a-project/
Starting an Open Source Project | Open Source Guides
https://docs.github.com/en/communities/setting-up-your-project-for-healthy-contributions/setting-guidelines-for-repository-contributors
Setting guidelines for repository contributors - GitHub Docs
https://github.com/github/docs/contribute
Contribute to github/docs
https://github.com/github/docs/blob/4535fa5277cf165ee196b789d41cc1dac8c476a7/CONTRIBUTING.md
docs/CONTRIBUTING.md at 4535fa5277cf165ee196b789d41cc1dac8c476a7 · github/docs
https://github.com/github/docs/blob/main/CONTRIBUTING.md
docs/CONTRIBUTING.md at main · github/docs
https://opensource.guide/code-of-conduct/
Your Code of Conduct | Open Source Guides
https://docs.github.com/en/get-started/quickstart/create-a-repo#commit-your-first-change
Create a repo - GitHub Docs
https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/creating-a-repository-on-github/licensing-a-repository#where-does-the-license-live-on-my-repository

## Licensing

Licensing a repository - GitHub Docs
https://choosealicense.com/licenses/mit/
MIT License | Choose a License
https://choosealicense.com/licenses/apache-2.0/
Apache License 2.0 | Choose a License
