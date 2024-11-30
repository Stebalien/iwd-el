;;; iwd.el --- Manage IWD Networks -*- lexical-binding: t -*-

;; Copyright 2024 Leon Henrik Plickat <leonhenrik.plickat@stud.uni-goettingen.de>
;; Copyright 2024 Steven Allen <steven@stebalien.com>

;; Author: Leon Henrik Plickat <leonhenrik.plickat@stud.uni-goettingen.de>
;; URL: https://git.sr.ht/~leon_plickat/iwd-el
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: unix, network

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Major mode to interface with iwd, allowing you to manage wireless connections.

;;; Code:

(require 'dbus)

;; TODO: DBus methods that may be useful
;; -> use (y-or-no-p "Forget network XXX?") for deleting known networks
;; Add dwim command, which on enter connects/disconnects from network in list
;; Add command to connect to hidden network

(defgroup iwd nil
  "Customize `iwd-mode' settings."
  :prefix "iwd-"
  :group 'convenience)

(defcustom iwd-connected-symbol "->"
  "Symbol used to indicate a current connection."
  :type 'string
  :group 'iwd)

(defface iwd-known-network '((t :inherit (italic font-lock-comment-face)))
  "Face for known networks.")

(defface iwd-available-network '((t :inherit bold))
  "Face for available networks.")

(defface iwd-connected-network '((t :inherit success))
  "Face for the connected network.")

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
  "Extract powered devices from the OBJ-ALIST."
  (let ((filter (lambda (obj) (assoc "net.connman.iwd.Device" obj)))
	(format (lambda (obj) (let ((dev (assoc "net.connman.iwd.Device" obj)))
                           (append (list (car obj)
                                         (assoc "Name" dev)
                                         (assoc "Address" dev)
                                         (assoc "Powered" dev))
                                   ;; The device may not be powered. We could check, but that check
                                   ;; would be racy so we might as well call first and ignore errors
                                   ;; later.
                                   (ignore-errors
                                     (dbus-call-method :system iwd--dbus-service
                                       (car obj)
                                       "net.connman.iwd.Station"
                                       "GetOrderedNetworks")))))))
    (mapcar format (seq-filter filter obj-alist))))

(defun iwd--signal-strength-to-string (s)
  "Turn signal strength S as returned by iwd to string displayed in `iwd-mode'."
  (let ((dbm (/ s 100)))
    (if iwd-signal-show-dbm
	(concat (int-to-string dbm) " dBm")
      (propertize (cond ((>= dbm -30) "*****")
			((>= dbm -67) "****")
			((>= dbm -80) "***")
			((>= dbm -90) "**")
			(t "*"))
		  'face 'bold))))

(defun iwd--get-connectable-networks (obj-alist devices)
  "Return a list all networks we can connect to from the given OBJ-ALIST.

Device name is added to SSID if multiple devices are available and DEVICES is
non-nil."
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
  "Return a list of all known networks from the given OBJ-ALIST."
  (let ((filter (lambda (obj) (assoc "net.connman.iwd.KnownNetwork" obj)))
	(format (lambda (obj) (let* ((id (intern (car obj)))
				(network (assoc "net.connman.iwd.KnownNetwork" obj))
				(name (cdr (assoc "Name" network))))
			   (list name id)))))
    (mapcar format (seq-filter filter obj-alist))))

(defun iwd--get-visible-networks-row (obj-alist devices)
  "Return a list of all visible networks from the given OBJ-ALIST and DEVICES.

The returned list is formatted for use in `tabulated-list-mode'."
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
				(list id (vector connected (propertize
                                                            ssid
                                                            'face
                                                            (if (equal connected iwd-connected-symbol) 'iwd-connected-network 'iwd-available-network))
                                                 device-name sigqual security address))))))
    (mapcar format (seq-filter filter obj-alist))))

(defun iwd--get-known-networks-row (obj-alist visible-networks)
  "Return a list of all known networks from the given OBJ-ALIST.

The returned list is formatted for use in `tabulated-list-mode'. If specified,
networks in VISIBLE-NETWORKS are excluded."
  (let* ((extract-name (lambda (id) (last (delete "" (split-string id "/")))))
	 (visible-network-names (mapcar (lambda (obj) (funcall extract-name (symbol-name (car obj)))) visible-networks))
	 (filter (lambda (obj) (and (assoc "net.connman.iwd.KnownNetwork" obj)
				    (not (member (funcall extract-name (car obj))
						 visible-network-names)))))
	 (format (lambda (obj)  (let* ((id (intern (car obj)))
				       (known-network (assoc "net.connman.iwd.KnownNetwork" obj))
				       (ssid (cdr (assoc "Name" known-network)))
				       (security (cdr (assoc "Type" known-network))))
				  (list id (vector "" (propertize ssid 'face 'iwd-known-network) "" "" security ""))))))
    (mapcar format (seq-filter filter obj-alist))))

(defun iwd--list-entries ()
  "Generate the `iwd-mode' table entries."
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
				(user-error "Already connected to this network"))
			       ((not (> (length sig) 0))
				(user-error "This network is not available at this moment"))
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
                               (user-error "No (other) networks available"))))
                       (cadr (assoc selected-ssid networks))))))
    (symbol-name id-symbol)))

(defun iwd-forget--get-network-path ()
  "Returns the D-Bus object path of the network to forget.

In `iwd-mode' it uses table entry under cursor, otherwise it queries user for
SSID."
  (if (eq major-mode 'iwd-mode)
      ;; Get network-entry from selected table vector.  Raises error
      ;; with cursor on KnownNetworks and currently connected
      ;; networks.
      (let* ((id (tabulated-list-get-id))
             (entry (tabulated-list-get-entry))
             (conn (elt entry 0)))
        (when (length= conn 0)
          (user-error "Network not known"))
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
                            (user-error "No (other) networks available"))))
      (cadr (assoc selected-ssid networks)))))

(defun iwd-disconnect--get-device-paths ()
  "Returns some number of devices to disconnect.

In `iwd-mode' it uses table entry under cursor (returns the device of the
connected network under the cursor, if any). Otherwise, it returns all
devices."
  (if (eq major-mode 'iwd-mode)
      ;; Get network-entry from selected table vector.  Raises error
      ;; with cursor on KnownNetworks and currently connected
      ;; networks.
      (let* ((id (tabulated-list-get-id))
             (entry (tabulated-list-get-entry))
             (conn (elt entry 0))
             (name (elt entry 1))
             (dev (elt entry 2)))
        (unless (string= iwd-connected-symbol conn)
          (user-error "Not connected to `%s' (%s)" name dev))
        (list (dbus-get-property :system iwd--dbus-service
                (symbol-name id) "net.connman.iwd.Network" "Device")))
    (mapcar 'car (iwd--get-devices (iwd--get-obj-alist)))))

;;;###autoload
(defun iwd-connect (path)
  "Connect to the wireless network at PATH."
  (interactive (list (iwd-connect--get-network-path)))
  (dbus-call-method :system iwd--dbus-service
    path "net.connman.iwd.Network" "Connect"))

;;;###autoload
(defun iwd-forget (path)
  "Forget the wireless network at PATH."
  (interactive (list (iwd-forget--get-network-path)))
  (dbus-call-method :system iwd--dbus-service
    path "net.connman.iwd.KnownNetwork" "Forget"))

;;;###autoload
(defun iwd-scan ()
  "Scan for available networks."
  (interactive)
  (dolist (device (iwd--get-devices (iwd--get-obj-alist)))
    ;; Ignore errors because the device may not be powered, or we may already be scanning.
    ;; We call async because there's no need to block, we don't really care about the outcome.
    (ignore-errors
      (dbus-call-method-asynchronously :system iwd--dbus-service
        (car device) "net.connman.iwd.Station" "Scan" nil))))

;;;###autoload
(defun iwd-disconnect ()
  "Disconnect the selected network, or all networks if not in an `iwd' buffer."
  (interactive)
  (dolist (device (iwd-disconnect--get-device-paths))
    (dbus-call-method :system iwd--dbus-service
      device "net.connman.iwd.Station" "Disconnect")))

(defvar-keymap iwd-mode-map
  :doc "`iwd-mode' keymap."
  :parent tabulated-list-mode-map
  (kbd "c") #'iwd-connect
  (kbd "s") #'iwd-scan)

(defvar iwd--state-change-dbus-signals nil
  "Registered D-Bus state-change signal handlers.")

(defvar iwd--state-change-debounce-timer nil
  "Currently active state-change debounce timer.")

(defvar iwd--state-change-debounce-timeout nil
  "Time beyond which state-change signals will not be delayed.")

(defconst iwd--state-change-debounce-min 0.1
  "Minimum time between `iwd-mode' buffer updates triggered by D-Bus signals.")

(defconst iwd--state-change-debounce-max 1.0
  "Maximum time between `iwd-mode' buffer updates triggered by D-Bus signals.")

(defun iwd--signal-handler-finish ()
  "Handles change signals from iwd and updates the iwd buffer accordingly."
  (setq iwd--state-change-debounce-timer nil
        iwd--state-change-debounce-timeout nil)
  (if-let* ((buf (get-buffer iwd--buffer-name)))
      (with-current-buffer buf
        (tabulated-list-revert))
    (iwd--unregister-signal-handler)))

(defun iwd--signal-handler (&rest _ignore)
  "Handles change signals from iwd.

Signals are debounced them and eventually calling
`iwd--signal-handler-finish'."
  (unless iwd--state-change-debounce-timeout
    (setq iwd--state-change-debounce-timeout
          (+ (float-time) iwd--state-change-debounce-max))
  (when (and iwd--state-change-debounce-timer
             (< (float-time) iwd--state-change-debounce-timeout))
    (cancel-timer iwd--state-change-debounce-timer)
    (setq iwd--state-change-debounce-timer nil))
  (unless iwd--state-change-debounce-timer
    (setq iwd--state-change-debounce-timer
          (run-with-timer iwd--state-change-debounce-min
                          nil #'iwd--signal-handler-finish)))))

(defun iwd--register-signal-handler ()
  "Registers a D-Bus signal handler for iwd state-change events."
  (unless iwd--state-change-dbus-signals
    (dolist (m '("InterfacesAdded" "InterfacesRemoved"))
      (push
       (dbus-register-signal
        :system iwd--dbus-service
        "/" dbus-interface-objectmanager m
        #'iwd--signal-handler)
       iwd--state-change-dbus-signals))))

(defun iwd--unregister-signal-handler ()
  "Unregisteres D-Bus signal handlers for iwd state-change events."
  (dolist (obj iwd--state-change-dbus-signals)
    (dbus-unregister-object obj))
  (setq iwd--state-change-dbus-signals nil))

(define-derived-mode iwd-mode tabulated-list-mode iwd--mode-name
  "Major mode for interfacing with iwd."
  (setq tabulated-list-format iwd--list-format
	tabulated-list-entries #'iwd--list-entries
	tabulated-list-padding 0
	tabulated-list-sort-key (cons "Device" t))
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode))

(defconst iwd--agent-path (concat dbus-path-emacs "/iwd_agent"))

(defun iwd--get-network-name (path)
  "Get the network name of PATH."
  (dbus-get-property :system iwd--dbus-service
    path "net.connman.iwd.Network" "Name"))

(defvar iwd--agent-registered nil
  "Non-nil when the IWD agent has been registered.")

(defun iwd--agent-release ()
  "Release the IWD agent.

Part of the `net.connman.iwd.Agent' interface."
  (setq iwd--agent-registered nil))

(defun iwd--agent-request-passphrase (path)
  "Request the passphrase for the network at PATH.

Part of the `net.connman.iwd.Agent' interface."
  (list (read-passwd (format "[iwd] Network passphrase for '%s': " (iwd--get-network-name path)))))

(defun iwd--agent-request-private-key-passphrase (path)
  "Request the passphrase for the private key associated with the network at PATH.

Part of the `net.connman.iwd.Agent' interface."
  (list (read-passwd (format "[iwd] Private key passphrase for '%s': " (iwd--get-network-name path)))))

(defun iwd--agent-request-user-name-and-password (path)
  "Request the user name and password for the network at PATH.

Part of the `net.connman.iwd.Agent' interface."
  (let* ((netw (iwd--get-network-name path))
         (user (read-string (format "[iwd] User Name for '%s' [default: %s]: " netw user-login-name)
                            nil nil user-login-name))
         (pass (read-passwd (format "[iwd] Password for '%s' @ '%s': " user netw))))
    (list user pass)))

(defun iwd--agent-cancel (reason)
  "Cancel an agent request for REASON.

Part of the `net.connman.iwd.Agent' interface."
  (message "[iwd] Connection attempt canceled: %s" reason))

(defconst iwd--agent-methods
  '(("Release" . iwd--agent-release)
    ("RequestPassphrase" . iwd--agent-request-passphrase)
    ("RequestPrivateKeyPassphrase" . iwd--agent-request-private-key-passphrase)
    ("RequestUserNameAndPassword" . iwd--agent-request-user-name-and-password)
    ("Cancel" . iwd--agent-cancel)))

(defvar iwd--agent-method-objects nil
  "Registered method objects for the IWD agent.")

(defun iwd--ensure-agent ()
  "Create the IWD agent if it doesn't already exist."
  (unless iwd--agent-method-objects
    (setq iwd--agent-method-objects
          (mapcar (pcase-lambda (`(,method . ,handler))
                    (dbus-register-method
                     :system nil iwd--agent-path "net.connman.iwd.Agent" method handler 'dont-register))
                  iwd--agent-methods))))

(defun iwd--destroy-agent ()
  "Destroy the IWD agent, if it exists."
  (when iwd--agent-method-objects
    (iwd--unregister-agent)
    (dolist (obj iwd--agent-method-objects)
      (dbus-unregister-object obj))
    (setq iwd--agent-method-objects nil)))

(defun iwd--register-agent ()
  "Register the IWD agent."
  (unless iwd--agent-registered
    (iwd--ensure-agent)
    (dbus-call-method :system iwd--dbus-service
      iwd--dbus-path "net.connman.iwd.AgentManager" "RegisterAgent"
      :object-path iwd--agent-path)
    (setq iwd--agent-registered t)))

(defun iwd--unregister-agent ()
  "Unregister the IWD agent."
  (when iwd--agent-registered
    (dbus-call-method :system iwd--dbus-service
      iwd--dbus-path "net.connman.iwd.AgentManager" "UnregisterAgent"
      :object-path iwd--agent-path)
    (setq iwd--agent-registered nil)))

;;;###autoload
(define-minor-mode iwd-agent-mode
  "Agent for the iNet Wireless Daemon (iwd).

When enabled, Emacs will prompt the user for network credentials on behalf of
iwd."
  :global t
  (if iwd-agent-mode
      (iwd--register-agent)
    (iwd--unregister-agent)))

;;;###autoload
(defun iwd ()
  "Control iwd WLAN connections."
  (interactive)
  (with-current-buffer (switch-to-buffer iwd--buffer-name)
    (iwd--register-signal-handler)
    (unless (derived-mode-p 'iwd-mode)
      (erase-buffer)
      (iwd-mode))))

(provide 'iwd)
;;; iwd.el ends here
