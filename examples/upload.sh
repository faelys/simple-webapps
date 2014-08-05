#!/bin/sh

# Copyright (c) 2014, Natacha Porté
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

# This is a helper script for the client side of Upload_Servers. It can
# upload a local file using `curl` and compute its download link, or return
# the download link directly from the report URL or the base-64 digest of
# the file.

# The HMAC secret is either entered directly from standard input (which is
# vulnerable to other people looking at the screen) or from `pinentry`
# utility that is part of GnuPG project (which is vulnerable to executable
# hijacking).

: ${EXPIRE:=30 days}
: ${RATE_LIMIT=${3:-30k}}
: ${ROOT_URI:=http://upload.instinctive.eu/}

if test $# -lt 1; then
	cat >&2 <<-EOD
	Usage: $0 file [comment [rate-limit]]
	   or: $0 report-hash [filename]
	Environment variables:
	   EXPIRE      "number unit" expiration time, defaults to 30 days
	   HEX_SHA1    path to command that outout 40-hex-character SHA-1 sum
	   HMAC_SHA1   path to hmac-sha1 tool
	   PINENTRY    path to pinentry command (otherwise read on stdin)
	   RATE_LIMIT  upload rate limit given to curl (defaults to 30 kB/s)
	   ROOT_URI    upload server root
	EOD
	exit 1;
fi

if test -f $1; then
	if test -n "${HEX_SHA1}"; then
		HEX_HASH=$(${HEX_SHA1} "$1")
	elif test -x /sbin/sha1; then
		# FreeBSD base sha1 tool
		HEX_HASH=$(/sbin/sha1 -q "$1")
	elif which sha1sum 1>/dev/null 2>/dev/null; then
		# GNU sha1sum tool in PATH
		HEX_HASH=$(sha1sum "$1" | cut -c 1-40)
	else
		echo "Unable to detect SHA-1 tool, please set HEX_SHA1" >&2
		exit 1
	fi
	
	if which base64 1>/dev/null 2>/dev/null; then
		# GNU coreutils base64
		HASH=$(echo "${HEX_HASH}" |\
		     xxd -r -p |\
		     base64 |\
		     sed -e 'y,+/,-_,' -e 's/=//g')
	elif which b64encode 1>/dev/null 2>/dev/null; then
		# FreeBSD base-64 encoder
		HASH=$(echo "${HEX_HASH}" |\
		     xxd -r -p |\
		     b64encode - |\
		     sed -n -e 'y,+/,-_,' -e '2s/=//gp')
	else
		echo "Unable to find base-64 encoding tool" >&2
		exit 1
	fi

	EXPIRE_NUMBER="${EXPIRE% *}"
	if echo "${EXPIRE_NUMBER}" | grep '^[0-9]'; then
		echo Invalid expiration number \"${EXPIRE_NUMBER}\" >&2
		exit 1
	fi

	EXPIRE_UNIT="${EXPIRE#* }"
	if ! test "${EXPIRE_UNIT}" = seconds \
	    -o "${EXPIRE_UNIT}" = minutes \
	    -o "${EXPIRE_UNIT}" = hours \
	    -o "${EXPIRE_UNIT}" = days \
	    -o "${EXPIRE_UNIT}" = weeks
	then
		echo "Invalid expiration unit \"${EXPIRE_UNIT}\"" >&2
		exit 1
	fi

	BASE_NAME=$(basename "$1")
	curl -F file=@"$1;type=$(file -i "$1" | sed 's/^.*: //')" \
	    -F "expire=${EXPIRE_NUMBER}" \
	    -F "expire_unit=${EXPIRE_UNIT}" \
	    -F comment="${2:-Uploaded by script}" \
	    -F submit=Send \
	    ${RATE_LIMIT:+--limit-rate} ${RATE_LIMIT} \
	    "${ROOT_URI}post" \
	    || exit
else
	HASH="${1##*/}"
	BASE_NAME="$2"
fi

echo "Report at ${ROOT_URI}${HASH}"

if ! test -n "${HMAC_SHA1}"; then
	if which hmac-sha1 1>/dev/null 2>/dev/null; then
		HMAC_SHA1=hmac-sha1
	elif test -x $(dirname "$0")/hmac-sha1; then
		HMAC_SHA1=$(dirname "$0")/hmac-sha1
	else
		echo "Unable to detect HMAC-SHA-1 tool," \
		    "please set HMAC_SHA1" >&2
		exit
	fi
fi

HMAC=$(${HMAC_SHA1} -b "${PINENTRY:+-p}${PINENTRY:--f-}" "${HASH}" \
    | sed -e 'y,+/,-_,' -e 's/=//g')
echo "Download at ${ROOT_URI}${HMAC}/${BASE_NAME:-filename}"
