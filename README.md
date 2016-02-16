# Simple Web Applications (in Ada)

This repository contains a collection of simple independent web
applications, based on AWS.

## Append Server

The "Append Server" is one of the simple possible web applications: give an
append-only access to local files through POST endpoints.

In more details, each active endpoint is associated with a local file,
expected form-like POST request with a `data` field containing the data to
add and a `signature` field, containing a HAMC-SHA256 signature of the data
to authenticate it. Requests outside of known active endpoints are matched
against a given static file hierarchy, that may or may not contain a file
updated by an active endpoint.

## Upload Server

One of the easiest ways to copy a file from one computer to another is to
use a HTTP server as intermediary. `Simple_Webapps.Upload_Servers` provides
an AWS dispatcher implementing such a server.

### Basic Use

Once deployed (see below), the expected usage of the upload server is in
two steps, first upload then download.

Upload step:

  1. Load the server root form page
  2. Select the local file to upload, expiration delay and comment
  3. Once upload is complete, you're redirected to a report page,
     whose URL is the base-64 encoding of the file hash digest

Download step:

  1. Use the base-64 encoding of the file digest and the server secret
     to compute the HMAC, whose base-64 encoding is the download address
  2. Append a slash (`/`) and any name you like to the HMAC
  3. Use a browser or any HTTP client to download the file

### Rationale

The main consideration behind the scheme explained above is to prevent
abuse (an unrestricted open file sharing service is likely to end up
massively used for illegal purposes) while still allowing anybody to easily
send a file to the service owner or their trustees, and allowing the
service owner to easily make a file available to anybody.

The HMAC is used to prevent anybody from guessing a valid download link,
even after having seen several valid ones.

The file digest is HMAC'ed rather than the file itself, so that a link to a
report page is enough for the service owner to deduce the download link.

To help manage server resources, all uploaded files have an expiration
time, selected by the uploader. The expiration delay is bounded by a
value inversely proportional to the file size, so that larger files expire
sooner. The proportionality constant is a configurable server-wide value.

Note that it does not actually limit the disk usage of the upload server,
since the number of uploaded files is still unbounded. For reliability
reasons, nothing is done on application level to limit disk usage, the
administrator should use filesystem tools instead.

### Configuration

#### Application configuration file

An example configuration file, demonstrating all available options, is
provided in the repository as `examples/upload.sx`. It is formatted as
a [S-expression](http://people.csail.mit.edu/rivest/Sexp.txt).

The file is read as a list, whose atoms and lists not starting with an
atom are ignored. The following configuration keywords are recognised as
configurable variables:

  - `backend` followed by the path of the S-expression file where
    persistent information about available files is stored, optionally
    followed by S-expression pretty printer configuration
  - `directory` followed by the path of the directory where incoming file
    are stored (they are named after their hash digest)
  - `error-template` followed by the path of the AWS template used to
    render HTTP error responses
  - `hmac-key` followed by the secret used to make download links
  - `index-template` followed by the path of the AWS template used to
    render the root index page, with the upload form
  - `input-directory` followed by the path where AWS or nginx stores
    incoming files before they are processed by the application
  - `max-expiration` followed by a number, optionally followed by size.time
    unit, sets the server-wide constant used to compute maximum expiration
    delay
  - `report-template` followed by the path of the AWS template used to
    render the report pages
  - `static-resources` followed by the path of the directory where static
    files are searched when no application page match the requested URL

#### Templates

In the index template, the following variables are available:

  - `MAX_EXPIRATION_BYTE_SECONDS`: the raw number used for maximum
    expiration delay, in byte×seconds (that makes it quite large)
  - `MAX_EXPIRATION`: the number user for maximum expiration delay, scaled
    down to (hopefully) human-manageable levels
  - `MAX_EXPIRATION_UNIT`: the unit in which `MAX_EXPIRATION` is expressed,
    for example `GB.h` for gigabyte×hours
  - `MAX_EXPIRATION_PREFIX`: the SI prefix (in a power-of-two-scale) in the
    size component of `MAX_EXPIRATION_UNIT` (`G` in the previous example)
  - `MAX_EXPIRATION_SUFFIX`: the letter used as time symbol in
    `MAX_EXPIRATION_UNIT`, and is one of `s`, `m`, `h`, `d` or `w`.

Moreover, in the index page, the following form fields are expected:

  - `comment`: user-provided comment (displayed in raw text)
  - `expire`: expiration delay value
  - `expire_unit`: the time unit in which `expire` value is
    expressed, and must one of `seconds`, `minutes`, `hours`, `days` or
    `weeks`
  - `file`: the uploaded file

In the file report template, the following variables are available:

  - `COMMENT`: user-provided comment associated with the file
  - `DEBUG`: boolean indicating whether debug mode is activated
  - `DIGEST`: file hash digest, in hexadecimal
  - `DIGEST_TYPE`: algorithm used for hash digest and HMAC (currently
    SHA-1)
  - `DOWNLOAD_KEY` (only available in debug mode): HMAC value used to
    download the file
  - `DOWNLOAD_PATH` (only available in debug mode): absolute path that can
    be used to download file (equal to /`DOWNLOAD_KEY`/`NAME`)
  - `EXPIRATION`: file expiration time
  - `EXPIRATION_DELAY`: delay from now to expiration time, in
    human-readable name and units
  - `MIME_TYPE`: file type provided on upload
  - `NAME`: file name provided on upload
  - `UPLOAD`: upload time

In the error page template, the following variables are available:

  - `CODE`: three-digit decimal code of the HTTP error status
  - `MESSAGE`: message associated with the error code (e.g. `Not found`
    when `CODE` is 404)

#### Nginx upload module

The upload server has been written to work seamless with any of AWS upload
facilities or nginx upload module.

Here is an example of the latter:

	server {
	    listen 80;
	    server_name upload.instinctive.eu;
	    access_log /var/log/nginx/upload.log main;
	
	    client_max_body_size 1000m;
	
	    location /post {
	        upload_pass /nginx-post;
	        upload_store /upload/incoming;
	        upload_set_form_field $upload_field_name.name "$upload_file_name";
	        upload_set_form_field $upload_field_name.content_type "$upload_content_type";
	        upload_set_form_field $upload_field_name.path "$upload_tmp_path";
	        upload_aggregate_form_field "$upload_field_name.sha1" "$upload_file_sha1";
	        upload_aggregate_form_field "$upload_field_name.size" "$upload_file_size";
	        upload_pass_form_field "^submit$|^comment$|^expire.*$";
	        upload_store_access user:rw group:rw all:r;
	        upload_cleanup 400 404 499 500-505;
	    }
	    location /nginx-post {
	        proxy_pass http://172.30.1.8:8080;
	    }
	    location / {
	        proxy_pass http://172.30.1.8:8080;
	    }
	}
