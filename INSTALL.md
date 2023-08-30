### [Emacs](https://www.gnu.org/software/emacs/)

#### Install in Emacs 28+

Dracula is available in [NonGNU Elpa](https://elpa.nongnu.org/nongnu/dracula-theme.html) and thus, will be directly available starting from that version of Emacs.

```el
M-x package-install <RET> dracula-theme
```

#### Install using MELPA

First, make sure [to have enabled MELPA repository in your configuration](https://melpa.org/#/getting-started);

Then, you will find dracula in the regular package listing.

```el
M-x package-install <RET> dracula-theme
```

#### Install using Homebrew

```bash
    brew install emacs-dracula
```

#### Install manually

Add the emacs theme files to `~/.emacs.d/themes`;

To load a theme add the following to your `init.el`.

```el
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)
```

#### Configure

Some aspects of this theme are customizable. You can change them by doing `M-x customize-group dracula`. Then **restart** Emacs to apply them. 💜
