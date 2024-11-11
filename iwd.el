(require 'dbus)

;; TODO: DBus methods that may be useful
;; net.connman.iwd.KnownNetwork: Forget
;; net.connman.iwd.Network: Connect
;; How to connect password prompt?
;; -> use (read-passwd "Password for network XXX")
;; -> use (y-or-no-p "Forget network XXX?") for deleting known networks
;; Add dwim command, which on enter connects/disconnects from network in list
;; Add command to connect to hidden network

(defgroup iwd nil "Customize iwd-mode settings.")

(defcustom iwd-connected-symbol "->"
  "Symbol used to indicate a current connection."
  :type 'string
  :group 'iwd)

(defcustom iwd-known-symbol "+"
  "Symbol used to indicate a known network."
  :type 'string
  :group 'iwd)

(defcustom iwd-signal-show-dbm nil
  "Display signal strength in dBm instead of symbolic indicator."
  :tag "Show signal strenght in dBm"
  :type 'boolean
  :group 'iwd)

(defconst iwd--mode-name "iwd")
(defconst iwd--buffer-name "*WLAN*")
(defconst iwd--dbus-service "net.connman.iwd")
(defconst iwd--dbus-path "/net/connman/iwd")

(defun iwd--get-obj-alist ()
  "Get all toplevel DBus objects related to iwd."
  (dbus-get-all-managed-objects :system
    iwd--dbus-service
    iwd--dbus-path))

(defun iwd--get-devices (obj-alist)
  "Extract devices from the object alist."
  (let ((filter (lambda (obj) (assoc "net.connman.iwd.Device" obj)))
	(format (lambda (obj) (append (list (car obj)
					    (assoc "Name" (assoc "net.connman.iwd.Device" obj))
					    (assoc "Address" (assoc "net.connman.iwd.Device" obj)))
				      (dbus-call-method :system iwd--dbus-service
							(car obj)
							"net.connman.iwd.Station"
							"GetOrderedNetworks")))))
    (mapcar format (seq-filter filter obj-alist))))

(defun iwd--signal-strength-to-string (s)
  "Turn signal strength as returned by iwd to string displayed in iwd-mode."
  (let ((dbm (/ s 100)))
    (if iwd-signal-show-dbm
	(concat (int-to-string dbm) " dBm")
      (propertize (cond ((>= dbm -30) "*****")
			((>= dbm -50) "****")
			((>= dbm -67) "***")
			((>= dbm -75) "**")
			(t "*"))
		  'face 'bold))))

(defun iwd--get-connectable-networks (obj-alist devices)
  "Return list of all networks which we could connect to. Device name is added to SSID if multiple devices are available."
  (let ((filter (lambda (obj) (let* ((network (assoc "net.connman.iwd.Network" obj))
				     (connected (assoc "Connected" network)))
				(and network (not (cdr connected))))))
	(format (lambda (obj) (let* ((id (intern (car obj)))
				     (network (assoc "net.connman.iwd.Network" obj))
				     (ssid (cdr (assoc "Name" network)))
				     (name (if (> (length devices) 1)
					       (let* ((device-obj (assoc (cdr (assoc "Device" network)) devices))
						      (device-name (cdr (assoc "Name" device-obj))))
						 (concat ssid " (" device-name ")"))
					     ssid)))
				(list name id)))))
    (mapcar format (seq-filter filter obj-alist))))

(defun iwd--get-known-networks (obj-alist)
  "Return list of all known networks."
  (let ((filter (lambda (obj) (assoc "net.connman.iwd.KnownNetwork" obj)))
	(format (lambda (obj) (let* ((id (intern (car obj)))
				(network (assoc "net.connman.iwd.KnownNetwork" obj))
				(name (cdr (assoc "Name" network))))
			   (list name id)))))
    (mapcar format (seq-filter filter obj-alist))))

(defun iwd--get-visible-networks-row (obj-alist devices)
  "Return list of all visible networks, formatted for use in tabulated-list-mode."
  (let ((filter (lambda (obj) (assoc "net.connman.iwd.Network" obj)))
	(format (lambda (obj) (let* ((id (intern (car obj)))
				     (network (assoc "net.connman.iwd.Network" obj))
				     (ssid (cdr (assoc "Name" network)))
				     (device-obj (assoc (cdr (assoc "Device" network)) devices))
				     (device-name (cdr (assoc "Name" device-obj)))
				     (security (cdr (assoc "Type" network)))
				     (connected (cond
						 ((cdr (assoc "Connected" network)) iwd-connected-symbol)
						 ((cdr (assoc "KnownNetwork" network)) iwd-known-symbol)
						 (t "")))
				     (address (if (cdr (assoc "Connected" network))
						  (cdr (assoc "Address" device-obj))
						""))
				     (sigqual (iwd--signal-strength-to-string (cadr (assoc (car obj) device-obj)))))
				(list id (vector connected (propertize ssid 'face 'bold) device-name sigqual security address))))))
    (mapcar format (seq-filter filter obj-alist))))

(defun iwd--get-known-networks-row (obj-alist visible-networks)
  "Return list of all known networks, formatted for use in tabulated-list-mode. Excludes visible networks."
  (let* ((extract-name (lambda (id) (last (delete "" (split-string id "/")))))
	 (visible-network-names (mapcar (lambda (obj) (funcall extract-name (symbol-name (car obj)))) visible-networks))
	 (filter (lambda (obj) (and (assoc "net.connman.iwd.KnownNetwork" obj)
				    (not (member (funcall extract-name (car obj))
						 visible-network-names)))))
	 (format (lambda (obj)  (let* ((id (intern (car obj)))
				       (known-network (assoc "net.connman.iwd.KnownNetwork" obj))
				       (ssid (cdr (assoc "Name" known-network)))
				       (security (cdr (assoc "Type" known-network))))
				  (list id (vector iwd-known-symbol (propertize ssid 'face 'italic) "" "" security ""))))))
    (mapcar format (seq-filter filter obj-alist))))

(defun iwd--list-entries ()
  "Generate the iwd-mode table entries."
  (let* ((obj-alist (iwd--get-obj-alist))
	 (devices (iwd--get-devices obj-alist))
	 (visible-networks (iwd--get-visible-networks-row obj-alist devices))
	 (known-networks (iwd--get-known-networks-row obj-alist visible-networks)))
    (append visible-networks known-networks)))

(defconst iwd--list-format
  (vector `("" ,(max (length iwd-connected-symbol)
                     (length iwd-known-symbol))
            nil)
	  '("SSID" 30 t)
	  '("Device" 13 t)
	  '("Signal" 6 t)
	  '("Security" 8 t)
	  '("Address" 20 t)))

(defun iwd-connect--get-network-path ()
  "In iwd mode uses table entry under cursor, other wise queries user for SSID."
  (let ((id-symbol (if (eq major-mode 'iwd-mode)
		       ;; Get network-entry from selected table vector.  Raises error
		       ;; with cursor on KnownNetworks and currently connected
		       ;; networks.
		       (let* ((id (tabulated-list-get-id))
			      (entry (tabulated-list-get-entry))
			      (conn (elt entry 0))
			      (sig (elt entry 3)))
			 (cond ((string-equal conn iwd-connected-symbol)
				(error "Already connected to this network."))
			       ((not (> (length sig) 0))
				(error "This network is not available at this moment."))
			       (t id)))
		     ;; Get list of connectable networks, then querry
		     ;; user for SSID. Raises error if no unconnected
		     ;; networks are available.
		     (let* ((obj-alist (iwd--get-obj-alist))
			    (devices (iwd--get-devices obj-alist))
			    (networks (iwd--get-connectable-networks obj-alist devices))
			    (ssids (mapcar (lambda (x) (car x)) networks))
                            (selected-ssid
                             (if (> (length ssids) 0)
                                 (completing-read "Connect to network: " ssids
                                                  nil 'must-match)
                               (error "No (other) networks available."))))
                       (cadr (assoc selected-ssid networks))))))
    (symbol-name id-symbol)))

(defun iwd-forget--get-network-path ()
  "In iwd mode uses table entry under cursor, other wise queries user for SSID."
  (if (eq major-mode 'iwd-mode)
      ;; Get network-entry from selected table vector.  Raises error
      ;; with cursor on KnownNetworks and currently connected
      ;; networks.
      (let* ((id (tabulated-list-get-id))
             (entry (tabulated-list-get-entry))
             (conn (elt entry 0))
             (sig (elt entry 3)))
        (when (length= conn 0)
          (error "Network not known"))
        (dbus-get-property :system iwd--dbus-service (symbol-name id)
          "net.connman.iwd.Network" "KnownNetwork"))
    ;; Get list of known networks, then query user for SSID. Raises error if known
    ;; networks are available.
    (let* ((obj-alist (iwd--get-obj-alist))
           (networks (iwd--get-known-networks obj-alist))
           (ssids (mapcar (lambda (x) (car x)) networks))
           (selected-ssid (if (> (length ssids) 0)
                              (completing-read "Forget network: " ssids
                                               nil 'must-match)
                            (error "No (other) networks available."))))
      (cadr (assoc selected-ssid networks)))))

(defun iwd-connect (path)
  (interactive (list (iwd-connect--get-network-path)))
  (message path)
  ;; TODO XXX actually connect to network
  )

(defun iwd-forget (path)
  (interactive (list (iwd-forget--get-network-path)))
  (dbus-call-method :system iwd--dbus-service
    path "net.connman.iwd.KnownNetwork" "Forget"))

(defvar iwd-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "c") #'iwd-connect)
    map)
  "iwd mode keymap.")

(define-derived-mode iwd-mode tabulated-list-mode
  iwd--mode-name
  "Major mode for interfacing with iwd."
  (setq tabulated-list-format iwd--list-format
	tabulated-list-entries #'iwd--list-entries
	tabulated-list-padding 0
	tabulated-list-sort-key (cons "Device" t))
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun iwd ()
  "Control iwd WLAN connections."
  (interactive)
  (with-current-buffer (switch-to-buffer iwd--buffer-name)
    (unless (derived-mode-p 'iwd-mode)
      (erase-buffer)
      (iwd-mode))))

(provide 'iwd)
