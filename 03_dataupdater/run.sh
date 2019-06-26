#!/usr/bin/env bash
set -e

RUNMODE=$AZURERI_RUNMODE

if [ ! -z "$1" ]; then
	RUNMODE=$1
fi

case $RUNMODE in

  ALL)
    echo "Updating all"
	Rscript all.aux.R || exit 1
    ;;

  AUX)
    echo "Updating auxiliarity tables only"
	Rscript run.aux.R || exit 1
    ;;

  USAGE)
    echo "Updating billingdata and RIUtilization tables only"
	Rscript run.usage.R || exit 1
    ;;

  ALL_WITH_REPORT)
    echo "Updating all with HTML report"
	REPORTNAME=report-$(date +%s).html
	Rscript -e "library(rmarkdown); rmarkdown::render(\"UpdateData.Rmd\", output_file=\"$REPORTNAME\")" || exit 1
	
	echo "Done"
	# send report to azure storage
	if [ ! -z "$AZURERI_STORAGEACCOUNT" ] && [ ! -z "$AZURERI_STORAGECONTAINER" ] && [ ! -z "$AZURERI_SASTOKEN" ]; then
		echo "Sending report"
		curl -X PUT -T ./${REPORTNAME} -H "x-ms-date: $(date -u)" -H "x-ms-blob-type: BlockBlob" \
			"https://${AZURERI_STORAGEACCOUNT}.blob.core.windows.net/${AZURERI_STORAGECONTAINER}/${REPORTNAME}${AZURERI_SASTOKEN}"
	fi
	
    ;;

  *)
    echo "Invalid runmode, valid options: ALL, AUX, USAGE, ALL_WITH_REPORT"
	exit 1
    ;;
esac

exit 0