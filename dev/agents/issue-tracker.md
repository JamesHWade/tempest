# GitHub issue workflow

GitHub Issues in `JamesHWade/tempest` is the source of truth for the backlog.
Use `gh` from any checkout, with `--repo JamesHWade/tempest` to make the
repository explicit.

- Search open and closed issues before creating work. Prefer updating an
  existing issue when the scope matches.
- Use issue bodies for the current problem, scope, and acceptance criteria.
  Record implementation progress and verification in comments.
- Create parent/child relationships with GitHub sub-issues and blockers with
  GitHub issue dependencies. Related work can use ordinary issue links.
- Reference the issue from its PR. Close it when the accepted scope is verified
  and merged; keep incomplete work open with the remaining acceptance criteria.
- Pass multiline content through `--body-file` so Markdown remains intact.

```sh
gh issue list --repo JamesHWade/tempest --state open
gh issue list --repo JamesHWade/tempest --state all --search "keywords"
gh issue view NUMBER --repo JamesHWade/tempest --comments
gh issue create --repo JamesHWade/tempest --title "Problem to solve" --body-file issue.md
gh issue comment NUMBER --repo JamesHWade/tempest --body-file progress.md
```

## Historical Kata references

The [migration index](../migrations/kata-to-github.md) maps every former
`tempest#SHORT_ID` to its GitHub issue. Imported issue bodies retain original
text, comments, authors, timestamps, and closure evidence. Those historical
sections can describe superseded APIs; current decisions belong in subsequent
GitHub updates.

Use GitHub for all new activity. The old Kata ledger is retained for audit.
