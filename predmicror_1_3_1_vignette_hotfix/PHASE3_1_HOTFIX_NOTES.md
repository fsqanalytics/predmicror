# predmicror 1.3.1 vignette hotfix

This hotfix fixes a vignette build error in `inactivation-models.Rmd`.

The failing chunk used `plot(.fitted, .resid, data = aug)`. In base R, this
is not a formula interface and `data = aug` is ignored for resolving `.fitted`
and `.resid`, causing `object '.fitted' not found` during vignette rebuilding.

The fix uses explicit column extraction with `aug[[".fitted"]]` and
`aug[[".resid"]]`, and adds lightweight guards before drawing prediction lines.
