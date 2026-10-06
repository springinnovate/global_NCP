# Produce a Word document showing every difference between two versions of the paper as tracked
# changes (Review tab), with the original's comments carried over where their anchor text survives.
# Uses Word's own Compare (Review > Compare) through COM, so Word must be installed (Windows).
#
# Typical use: Original = the docx a reviewer commented on (downloaded from the online version
# history), Revised = the new render of paper_draft_5service.qmd. Formatting-only differences are
# ignored so Quarto's styling does not drown the real edits.
#
#   powershell -ExecutionPolicy Bypass -File scripts/compare_docx_tracked.ps1 `
#     -Original docs/manuscript/<reviewed>.docx -Revised docs/manuscript/<new>.docx `
#     -Output docs/manuscript/<new>_tracked_changes.docx -Author "Jeronimo Rodriguez (Oct 2026 revision)"

param(
    [Parameter(Mandatory = $true)][string]$Original,
    [Parameter(Mandatory = $true)][string]$Revised,
    [Parameter(Mandatory = $true)][string]$Output,
    [string]$Author = "Revision"
)

$ErrorActionPreference = "Stop"
$orig = (Resolve-Path $Original).Path
$rev  = (Resolve-Path $Revised).Path
$out  = [System.IO.Path]::GetFullPath($Output)

$word = New-Object -ComObject Word.Application
$word.Visible = $false
$word.DisplayAlerts = 0
try {
    $docO = $word.Documents.Open($orig, $false, $true)   # ConfirmConversions, ReadOnly
    $docR = $word.Documents.Open($rev, $false, $true)
    # CompareDocuments(Original, Revised, Destination=new doc, Granularity=word level,
    #   CompareFormatting, CaseChanges, Whitespace, Tables, Headers, Footnotes, Textboxes, Fields,
    #   Comments, RevisedAuthor, IgnoreAllComparisonWarnings)
    $result = $word.CompareDocuments($docO, $docR, 2, 1, $false, $true, $false, $true, $true, $true, $true, $true, $true, $Author, $true)
    $result.SaveAs2($out, 16)                             # 16 = wdFormatDocumentDefault (.docx)
    $nRev = $result.Revisions.Count
    $nCom = $result.Comments.Count
    $result.Close($false)
    $docO.Close($false)
    $docR.Close($false)
    Write-Output ("Saved {0}" -f $out)
    Write-Output ("Tracked changes: {0} | Comments carried over: {1}" -f $nRev, $nCom)
}
finally {
    $word.Quit()
    [System.Runtime.InteropServices.Marshal]::ReleaseComObject($word) | Out-Null
}
