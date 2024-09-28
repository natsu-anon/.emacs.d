## Windows Setup
1. [download](https://www.gnu.org/software/emacs/download.html#nonfree) (or use MSYS2 for the 64-bit goodness? wow.)
2. Set wherever you clone this repo to as the `$HOME` environment variable, traditionally in `C:\Users\Your Username\`
  - best way:
    1. run cmd as administrator
	2. run `setx home "C:\Users\Your Username"`
3. Add the emacs bin to your `$PATH` if you want to be able to use it from the command line (mb yes, mb no)
4. Create a shortcuts to launch emacs as a daemon, set it's target to `"Drive:\path\to\emacs\bin\runemacs.exe" --daemon`
5. Create a shortcuts to launch the emacs client, set it's target to `"Drive:\path\to\emacs\bin\emacsclientw.exe" -n -c`
6. After that you can do sexiness like adding it to your contextual open menu (use the client, it's fast af)
7. run `all-the-icons-install-fonts` inside emacs, then install the fonts manually (how it is).

## `~/.emacs.d/init.el`

``` emacs-lisp
(load-file "~/.emacs.d/config.el")
(load-file "~/.emacs.d/local.el")
;; (load-file "~/.emacs.d/gdscript.el") IF YOU WANT GDSCRIPT-MODE
```

Which will load `config.el`, the actual config, and `local.el`, local settings (which I use for looks).
Why like this?  It lets all the custom-set-variables and such accumulate in the init file and it doesn't get shared in the repo (anymore).

In order to get icons working you _MUST_ run `all-the-icons-install-fonts`, if you're on windows it just downloads the fonts so you have to manually install them.

### example `local.el`

``` emacs-lisp
(load-theme 'doom-monokai-classic t)
;; run menu-set-font to see the font menu.  Use describe-font <RET> to see the font being used
(add-to-list 'default-frame-alist '(font . "Cascadia Mono-10.0"))
(set-face-attribute 'default t :font "Cascadia Mono-10.0")

;; maximize initial frame
;; (add-hook 'window-setup-hook 'toggle-frame-maximized t)
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; works for clients as well
```

## USING CYGWIN AS SHELL ON WINDOWS
	
Include the following in  `local.el`

``` emacs-lisp
(setq shell-file-name "c:/cygwin64/bin/bash")
(setq explicit-shell-file-name shell-file-name)
(setq explicit-bash-args `("--noediting" "-i"))
```

### UPDATING PACKAGES

Don't do it automatically, packages change.
HOWTO:
1. `<M-x> package-list-packages` any packages available to upgrade will be shown.
2. Mark pacakges to update with `U`
3. Execute the upgrade with `x`

## HOW TO USE TRAMP ON WINDOWS

### Project `.gitignore`

add the following to your projects' `.gitignore` to ignore all the emacs backups and such:

```
# emacs stuff
\#*\#
.\#*
*~
.dir-locals.el
```

### making flycheck play with node well:

- Go to root directory of project and run `npm instal install eslint --save-dev`
  + If you're also using @babel stuff (you are if you copy the `.eslintrc.json` from below) you might also have to run `npm install @babel/core @babel/eslint-parser @babel/preset-env --save-dev`
- After that run `eslint --init` or `npx eslint --init`
- just do the basic setup, and save as a `json`
- after that replace `.eslintrc.json` with the following:

``` json
{
    "env": {
        "browser": false,
        "es2021": true,
		"node": true
    },
    "parser": "@babel/eslint-parser",
    "parserOptions": {
        "ecmaVersion": 12,
        "sourceType": "module"
    },
	"rules": {
		"getter-return": "error",
		"no-cond-assign": "error",
		"no-constant-condition": "error",
		"no-dupe-args": "error",
		"no-dupe-else-if": "error",
		"no-dupe-keys": "error",
		"no-duplicate-case": "error",
		"no-empty": "warn",
		"no-extra-boolean-cast": "warn",
		"no-func-assign": "error",
		"no-import-assign": "error",
		"no-setter-return": "error",
		"no-sparse-arrays": "warn",
		"no-unreachable": "warn",
		"no-unsafe-finally": "warn",
		"no-unsafe-negation": "error",
		"use-isnan": "error",
		"eqeqeq": [ "warn", "smart" ],
		"no-multi-spaces": "warn",
		"no-redeclare": "error",
		"no-useless-escape": "warn",
		"no-with": "error",
		"no-undef": "error",
		"no-unused-vars": [ "warn", { "vars": "local", "args": "all" }],
		"wrap-iife": ["warn", "inside"],
		"object-curly-spacing": ["warn", "always"],
		"quotes": ["warn", "single", { "avoidEscape": true, "allowTemplateLiterals": true }],
		"semi": ["error", "always"],
		"no-const-assign": "error",
		"arrow-spacing": "warn",
		"no-confusing-arrow": "warn",
		"no-dupe-class-members": "error",
		"no-duplicate-imports": "warn",
		"no-var": "warn",
		"no-confusing-arrow": "warn",
		"constructor-super": "error",
		"no-this-before-super": "error",
		"prefer-const": "warn",
		"require-yield": "warn",
		"arrow-parens": ["warn", "as-needed"]
	}
}
```
- create `.babelrc` with the following:

``` json
{
	"presets": [
		[ "@babel/preset-env",
		  {
			  "shippedProposals": true
		  }]
	]
}

```
- finally create `.dir-locals.el` with the following:

``` emacs-lisp
((js-mode . ((eval . (progn
				  (make-local-variable 'exec-path)
				  (add-to-list 'exec-path "/path/to/repo/node_modules/.bin/"))))))
```
idk why but I haven't gotten it to work well when emacs is started in i3 (not the client, but emacs itself)
try starting the server in a different DE/WM session then going into i3
