# P2P Fix — open this file to see results

Chat messages aren't rendering in this session (known Dispatch UI bug).
The fix is complete on disk. Open this file for the full summary.

## Push to a new branch (run in mgid repo)

```bash
git checkout -b fix/p2p-reversed-links
git add R/13_p2p.R R/03_group_data.R tests/testthat/test-p2p.R NEWS.md
git commit -m "fix(p2p): resolve links stored local-on-right via linxmart_p2p option"
git push -u origin fix/p2p-reversed-links
```

Note: only stages the 4 P2P files — your other local changes are left untouched.

---

## Root Cause

`MatchingPairExternal` rows where the **local record is on the RIGHT** were silently
dropped by `p2p_resolve_links()`. That is how your production data is stored, so
P2P returned nothing at work.

## Files Changed

### `R/13_p2p.R`
- `p2p_resolve_links()` gains `p2p_opt_pid` parameter
- Local-on-right rows now rescued using `linxmart_p2p` option's project ID
- `links` table gains `left_id`/`right_id` columns (original row orientation for mpe filter)

### `R/03_group_data.R`
- `get_linxmart_project_name()` wrapped in `tryCatch` (failure no longer crashes load)
- `p2p_opt_pid` computed from `linxmart_p2p` option and passed through
- `mpe` filter uses `left_id`/`right_id` — matches links in both orientations

### `tests/testthat/test-p2p.R`
- New test: resolves a local-on-right row when `p2p_opt_pid` is given

### `NEWS.md`
- Two new bullets documenting both fixes
