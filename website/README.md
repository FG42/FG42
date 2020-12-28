![Build Status](https://gitlab.com/pouya-abbassi/pouyacode/badges/master/build.svg)

---

[FG42](https://fg42.org) built using [Pelican](https://blog.getpelican.com) & [Bulma](https://bulma.io).

## Build

```
pip install -r requirements.txt
git clone git@github.com:getpelican/pelican-plugins.git
make publish
```

This will create `public` directory contaning all static files.

## Email Obfuscation
I use [ROT13](https://en.wikipedia.org/wiki/ROT13) to obfuscate email adresses.
So email adresses that would be visible to users should be generated using this command (or similar tools):

```
echo "pcode@protonmail.com" | tr 'A-Za-z' 'N-ZA-Mn-za-m'
#or
tr 'A-Za-z' 'N-ZA-Mn-za-m' <<< "pcode@protonmail.com"
```

They are then processed on user's browser using a simple js function.

## Sample Pages
There is a sample page at `content/docs.md` that has a metadata `status: hidden`.
Remove this metadata to see it in the index page. This page is just for demo purpose.

Also there are two pages inside `content/pages/` directory with metadata `error: true`.
This makes the background to stretch and fill the page.
The `content/pages/release-notes.md` is also just for demo purpose.

## Images
Images for articles and pages should be stored at `theme/static/images/articles/` and `theme/static/images/pages/` directory.
These images should be named after their page title so it would be easier to manage.

## Configs
Development config file is `pelicanconf.py` and the deployment config file is `publishconf.py`.
`SITEURL` and `PLUGINS` are two most important lines of those files.
