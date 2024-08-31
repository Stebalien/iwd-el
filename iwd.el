(require 'dbus)

;; TODO: DBus methods that may be useful
;; net.connman.iwd.KnownNetwork: Forget
;; net.connman.iwd.Network: Connect

(defcustom iwd-connected-symbol "->"
  "Symbol used to indicate a current connection."
  :type 'string)

(defconst iwd--mode-name "iwd")
(defconst iwd--buffer-name "*WLAN*")
(defconst iwd--dbus-service "net.connman.iwd")

(defun iwd--get-obj-alist ()
  (dbus-get-all-managed-objects :system
				iwd--dbus-service
				"/net/connman/iwd"))

(defun iwd--obj-alist-extract-devices (obj-alist)
  "Extract devices from the object alist"
  (let ((filter (lambda (obj) (assoc "net.connman.iwd.Device" obj)))
	(format (lambda (obj) (append (list (car obj)
					    (assoc "Name" (assoc "net.connman.iwd.Device" obj))
					    (assoc "Address" (assoc "net.connman.iwd.Device" obj)))
				      (dbus-call-method :system iwd--dbus-service
							(car obj)
							"net.connman.iwd.Station"
							"GetOrderedNetworks")))))
    (mapcar format (seq-filter filter obj-alist))))

(defun iwd--obj-alist-extract-visible-networks-row (obj-alist devices)
  (let ((filter (lambda (obj) (assoc "net.connman.iwd.Network" obj)))
	(format (lambda (obj) (let* ((id (intern (car obj)))
				     (network (assoc "net.connman.iwd.Network" obj))
				     (ssid (cdr (assoc "Name" network)))
				     (device-obj (assoc (cdr (assoc "Device" network)) devices))
				     (device-name (cdr (assoc "Name" device-obj)))
				     (security (cdr (assoc "Type" network)))
				     (connected (if (cdr (assoc "Connected" network))
						    iwd-connected-symbol
						  ""))
				     (address (if (cdr (assoc "Connected" network))
						  (cdr (assoc "Address" device-obj))
						""))
				     (sigqual (let ((sig (/ (cadr (assoc (car obj) device-obj)) 100)))
						(propertize (cond ((>= sig -30) "*****")
								  ((>= sig -50) "****")
								  ((>= sig -67) "***")
								  ((>= sig -75) "**")
								  (t "*"))
							    'face 'bold))))
				(list id (vector connected (propertize ssid 'face 'bold) device-name sigqual security address))))))
    (mapcar format (seq-filter filter obj-alist))))

(defun iwd--obj-alist-extract-known-networks-row (obj-alist visible-networks)
  (let* ((extract-name (lambda (id) (last (delete "" (split-string id "/")))))
	 (visible-network-names (mapcar (lambda (obj) (funcall extract-name (symbol-name (car obj)))) visible-networks))
	 (filter (lambda (obj) (and (assoc "net.connman.iwd.KnownNetwork" obj)
				    (not (member (funcall extract-name (car obj))
						 visible-network-names)))))
	 (format (lambda (obj)  (let* ((id (intern (car obj)))
				       (known-network (assoc "net.connman.iwd.KnownNetwork" obj))
				       (ssid (cdr (assoc "Name" known-network)))
				       (security (cdr (assoc "Type" known-network))))
				  (list id (vector ""  (propertize ssid 'face 'italic) "" "" security ""))))))
    (mapcar format (seq-filter filter obj-alist))))

(defun iwd--list-entries ()
  (let* ((obj-alist (iwd--get-obj-alist))
	 (devices (iwd--obj-alist-extract-devices obj-alist))
	 (visible-networks (iwd--obj-alist-extract-visible-networks-row obj-alist devices))
	 (known-networks (iwd--obj-alist-extract-known-networks-row obj-alist visible-networks)))
    (append visible-networks known-networks)))

(defconst iwd--list-format
  (vector `("" ,(length iwd-connected-symbol) nil)
	  '("SSID" 30 t)
	  '("Device" 13 t)
	  '("Signal" 6 t)
	  '("Security" 8 t)
	  '("Address" 20 t)))

(defun iwd-test (path)
  (interactive (iwd--select-connectable-network))
  (when path
    (message path)))

(defvar iwd-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "d") #'iwd-test)
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
