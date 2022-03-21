# Emacs config

Inspired by purcell's config

![Scrot](https://raw.githubusercontent.com/jilen/.emacs.d/main/scrot.png)

## What's inside
+ Improved general editing experience
+ [Projectile](https://github.com/bbatsov/projectile) like  `project.el` setup.
+ [Consult](https://github.com/minad/consult) and [Corfu](https://github.com/minad/corfu) based lightweight completion ui.
+ Useful programming setup.
  - Scala
  - Vue
  - ...

## Keymaps

### Project management

Keymap `project-prefix-map` rebinded to `C-c p` as `projectile`

| Binding | Action                             |
|---------|------------------------------------|
| C-c p p | Switch project                       |
| C-c p f | Open file in project                |
| C-c p s | Do ripegrep inside project         |
| C-c p r | Do search-replace-regex in project |

### Consult

| Binding | Action         | Desc                        |
|---------|----------------|-----------------------------|
| C-s     | consult-line   | Search in buffer            |
| C-x b   | consult-buffer | List recent buffers         |
| C-c h i | consult-imenu  | Show imenu item             |
| C-.     | embark-act     | Contextual actions an point |
