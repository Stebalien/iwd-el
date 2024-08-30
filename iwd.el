(require 'dbus)

;; TODO deduplicate network list

;; TODO: DBus methods that may be useful
;; net.connman.iwd.KnownNetwork: Forget
;; net.connman.iwd.Network: Connect

(defcustom iwd-connected-symbol "->"
  "Symbol used to indicate a current connection."
  :type 'string)

(defconst iwd--mode-name "WLAN (iwd)" "Pretty print mode name.")
(defconst iwd--dbus-service "net.connman.iwd")

(defun iwd--try-connect (id)
  (msg (symbol-name id)))

(defun iwd--list-entries ()
  (let* ((obj-alist (dbus-get-all-managed-objects :system
						  iwd--dbus-service
						  "/net/connman/iwd"))
	 (device-filter-p (lambda (obj) (assoc "net.connman.iwd.Device" obj)))
	 (device-formatter (lambda (obj) (append
					  (list (car obj)
						(assoc "Name" (assoc "net.connman.iwd.Device" obj))
						(assoc "Address" (assoc "net.connman.iwd.Device" obj)))
					  (dbus-call-method :system iwd--dbus-service
							    (car obj)
							    "net.connman.iwd.Station"
							    "GetOrderedNetworks"))))
	 (device-alist (mapcar device-formatter (seq-filter device-filter-p obj-alist)))
	 (network-filter-p (lambda (obj) (or (assoc "net.connman.iwd.KnownNetwork" obj)
					     (assoc "net.connman.iwd.Network" obj))))
	 (networks (seq-filter network-filter-p obj-alist))
	 (network-to-row (lambda (N)
			   (let ((id (intern (car N)))
				 (network (assoc "net.connman.iwd.Network" N))
				 (known-network (assoc "net.connman.iwd.KnownNetwork" N)))
			     (cond (network
				    (let* ((ssid (cdr (assoc "Name" network)))
					   (device-obj (assoc (cdr (assoc "Device" network)) device-alist))
					   (device (cdr (assoc "Name" device-obj)))
					   (security (cdr (assoc "Type" network)))
					   (connected (if (cdr (assoc "Connected" network))
							  iwd-connected-symbol
							""))
					   (address (if (cdr (assoc "Connected" network))
							(cdr (assoc "Address" device-obj))
						      ""))
					   (sigqual (let ((sig (/ (cadr (assoc (car N) device-obj)) 100)))
						      (cond ((>= sig -30) "*****")
							    ((>= sig -50) "****")
							    ((>= sig -67) "***")
							    ((>= sig -75) "**")
							    (t "*")))))
				      (list id (vector connected (propertize ssid 'face 'bold) device sigqual security address))))
				   (known-network
				    (let ((ssid (cdr (assoc "Name" known-network)))
					  (security (cdr (assoc "Type" known-network))))
				      (list id (vector  "" ssid (propertize "unavailable" 'face 'italic) "" security "")))))))))
    (mapcar network-to-row networks)))

(defconst iwd--list-format
  (vector `("" ,(length iwd-connected-symbol) nil)
	  '("SSID" 30 t)
	  '("Device" 13 t)
	  '("Signal" 6 t)
	  '("Security" 8 t)
	  '("Address" 20 t)))

(define-derived-mode iwd-ctl-mode tabulated-list-mode
  iwd--mode-name
  "Major mode for interfacing with  iwd."
  (setq tabulated-list-format iwd--list-format
	tabulated-list-entries #'iwd--list-entries
	tabulated-list-padding 0
	tabulated-list-sort-key (cons "Device" t))
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun iwd ()
  "Control iwd WLAN connections."
  (interactive)
  (with-current-buffer (switch-to-buffer iwd--mode-name)
    (unless (derived-mode-p 'iwd-ctl-mode)
      (erase-buffer)
      (iwd-ctl-mode))))

(provide 'iwd)
