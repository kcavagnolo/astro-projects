#gs -dBATCH -sDEVICE=pdfwrite -dNOPAUSE -sOUTPUTFILE=merged.pdf cover.pdf resume.pdf summary.pdf gt_transcript.pdf msu_transcript.pdf
gs -dBATCH -sDEVICE=pdfwrite -dNOPAUSE -sOUTPUTFILE=merged.pdf cover.pdf resume.pdf summary.pdf gt_transcript_1.gif gt_transcript_2.gif gt_transcript_3.gif msu_transcript.gif
open merged.pdf
