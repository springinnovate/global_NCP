Hi Becky, hi Rich,

Update on the Philippines comparison. Quickflow lands close to your baseline (~77%, r=0.56) and
it's fully explained — swapping in your CN alone reproduces your quickflow almost exactly (r=0.92),
and Kc doesn't affect quickflow at all in this model, so that's a clean, one-variable story: the gap
is just our CN choice vs. yours.

Baseflow looked worse at first (~148% of yours, wrong direction, r=0.39), but it turns out to have
just as clean a story once we looked closer. Your Kc alone explains most of the gap, your CN alone
explains some, and neither one alone fully closes it — correcting just one leaves a real, structured
gap that varies by land-cover class. But your CN and Kc *together* close it completely: r=0.92, same
as quickflow. So the whole thing comes down to the CN/Kc choice, both variables, nothing hidden or
unexplained left over. Full numbers and the scatter plots showing that: [report link/path —
swy_status_report.html].

Also worth knowing: we ran a full battery of checks (grid registration, resampling, valid-pixel
masking, a line-by-line audit against your `.ini`) specifically to rule out a hidden bug in our own
pipeline before trusting any of the above — all came back clean. That audit did surface two things
your `.ini` just doesn't specify (`beta_i`/`gamma`, and the DEM source) — we used standard defaults,
flagging in case either matters to you.

One honest caveat, and an actual question for you: this whole thing is a model-to-model comparison
— our output against yours, not against anything observed. So it tells us our pipeline is correct
and exactly which parameters are driving the difference, but not which of our two parametrizations
is actually closer to real Philippine hydrology. Has your own baseline ever been checked against
streamflow gauge data or anything observational? That would change a lot about how much weight we
can put on closing the gap to your numbers specifically, versus treating this as two unvalidated
estimates either way.

Rich — separately, real bugs in `seasonal_water_yield`, checked against a fresh clone of
`springinnovate/inspring` just now so I know they're not stale (last commit touching that file:
Feb 2025). Broken packaging, three bugs blocking real spatially-distributed rain events, a
checkerboard-causing resampling bug, and an undocumented-but-useful raster-override feature with its
own nodata bug. Full writeup with proposed fixes: `docs/swy/inspring_github_issues_draft.md` — happy
to open real issues/PRs if that's useful, or just hand you the diffs directly, whichever's easier on
your end.

Let me know what's most helpful from here.
