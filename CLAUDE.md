# CLAUDE.md - Emacs Configuration

This is an Emacs configuration using org-babel literate programming.

## Structure

- `init.org` - Main configuration file (literate org-mode)
- `init.el` - Compiled elisp (auto-generated from init.org via `org-babel-tangle`)

**Do not edit init.el directly** - it is overwritten when init.org is saved.

## Package Management

Uses `straight.el` with `use-package`. The setting `straight-use-package-by-default` is `t`, so `:ensure t` is redundant for most packages.

## Key Sections in init.org

| Section | Purpose |
|---------|---------|
| frontmatter | Bootstrap (straight, diminish, no-littering, org, evil) |
| packages | Third-party packages (dired, magit, vertico, corfu, etc.) |
| languages | Language-specific config (elisp, clojure, python, js/ts) |
| email | mu4e configuration for Fastmail and Gmail |
| itself | Core Emacs settings (`use-package emacs`) |
| functionaria | Custom elisp functions |
| osx specific | macOS-specific settings and keybindings |
| interface | Keybindings and registers |
| theme | Theme loading (ef-themes) |

## Auto-generated Files (do not commit frequently)

- `project-window-list` - project-x session state
- `gptel-crowdsourced-prompts.csv` - gptel prompts cache
- `var/` - savehist, recentf, etc. (managed by no-littering)

## Evil Mode

Evil is installed but **not enabled by default**. Toggle with `C-z`.

## Common Tasks

- **Reload config**: `C-x C-,` or `M-x load-init-file`
- **Edit config**: `C-x ,` or `M-x edit-init-org-file`
- **Tangle manually**: In init.org, run `M-x org-babel-tangle`

## Testing Changes

After editing init.org:
1. Save the file (auto-tangles to init.el)
2. Run `M-x load-init-file` to reload
3. Or restart Emacs to test fresh

## Known Issues

- Copilot node path may need updating if node version changes
- Some deprecated functions remain (toggle-read-only, linum-mode)
- org-catch-invisible-edits has invalid value (should be symbol, not t)
