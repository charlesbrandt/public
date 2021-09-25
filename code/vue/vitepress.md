# Vitepress

Where have you been? Such a great system for writing documentation! The refresh is so fast -- it's a live version of the application running!

## Publishing

And then, when it's time to deploy, it include Server Side Generation (SSG). Gitlab and Github will host the output from this for you. 

Overview of the process for Gitlab

https://docs.gitlab.com/ee/user/project/pages/getting_started/pages_from_scratch.html

Adapting from:

https://gitlab.com/pages/vuepress/-/blob/master/.gitlab-ci.yml

Using a submodule, so be sure to enable those:


https://docs.gitlab.com/ee/ci/git_submodules.html


```
image: node:lts

variables:
  GIT_SUBMODULE_STRATEGY: recursive
  
pages:
  cache:
    paths:
    - node_modules/
  script:
  - yarn install
  - yarn run build
  artifacts:
    paths:
    - public
  rules:
    - if: $CI_COMMIT_REF_NAME == $CI_DEFAULT_BRANCH
```

Then, once you commit and push the `.gitlab-ci.yml`, you can log in to gitlab to see the status of the job

https://docs.gitlab.com/ee/ci/jobs/
