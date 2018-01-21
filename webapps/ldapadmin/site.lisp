;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

;; ========================================================================== ;;

(defmacro .base ()
  `(html5
    `(html
      (head
       ((meta :name "viewport" :content "width=device-width, initial-scale=1"))
       ((meta :charset "utf-8"))
       ((title) ,(title *webapp*))
       ,@(mapcar (lambda (css)
                   `((link :rel "stylesheet" :href ,css)))
                 '("https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css")))
       ,@(mapcar (lambda (js)
                   `((script :type "text/javascript" :src ,js)))
                 '("https://ajax.googleapis.com/ajax/libs/jquery/3.2.0/jquery.min.js"
                   "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                   "/static/js/cljs/main.js"))
      (body
       ((div :class "container-fluid")
        ((div :class "page-header")
         ((h2 :align "center") ,(title *webapp*)))
        ((div :id "menu" :class "well"))
        ((div :id "location"))
        ((div :id "errormsg"))
        ((div :id "message"))
        ((div :id "body")))))))

;; ========================================================================== ;;

(defmacro .location ()
  `(location-json))

(defmacro .home ()
  `(home-json))

(defmacro .menu ()
  `(menu-json))

(defmacro .login ()
  `(login-json))

(defmacro .login-authenticate ()
  `(login-authenticate-json dn password))

(defmacro .logout ()
  `(logout-json))

(defmacro .inetorg-view ()
  `(inetorg-view-json))

(defmacro .inetorg-view-search ()
  `(inetorg-view-search-json))

(defmacro .inetorg-view-results ()
  `(inetorg-view-results-json givenname sn mail postaladdress postalcode st l telephonenumber mobile))

(defmacro .inetorg-modify ()
  `(inetorg-modify-json cn))

(defmacro .inetorg-modify-submit ()
  `(inetorg-modify-submit-json givenname sn mail postaladdress postalcode st l telephonenumber mobile))

(defmacro .inetorg-delete ()
  `(inetorg-delete-json cn))

(defmacro .inetorg-delete-submit ()
  `(inetorg-delete-submit-json cn))

(defmacro .inetorg-add ()
  `(inetorg-add-json))

(defmacro .inetorg-add-submit ()
  `(inetorg-add-submit-json givenname sn mail postaladdress postalcode st l telephonenumber mobile))

;; ========================================================================== ;;

(def-page :get "/" () .base)
(def-page :get "/location" () .location)
(def-page :get "/home" () .home)
(def-page :get "/menu" () .menu)
(def-page :get "/login" () .login)
(def-page :post "/login/authenticate" ((dn :parameter-type 'string) (password :parameter-type 'string)) .login-authenticate)
(def-page :get "/logout" () .logout)
(def-page :get "/inetorg/view" () .inetorg-view)
(def-page :get "/inetorg/view/search" () .inetorg-view-search)
(def-page :post "/inetorg/view/results" ((givenname :parameter-type 'string) (sn :parameter-type 'string) (mail :parameter-type 'string) (postaladdress :parameter-type 'string) (postalcode :parameter-type 'string) (st :parameter-type 'string) (l :parameter-type 'string) (telephonenumber :parameter-type 'string) (mobile :parameter-type 'string)) .inetorg-view-results)
(def-page :post "/inetorg/modify" ((cn :parameter-type 'string)) .inetorg-modify)
(def-page :post "/inetorg/modify/submit" ((givenname :parameter-type 'string) (sn :parameter-type 'string) (mail :parameter-type 'string) (postaladdress :parameter-type 'string) (postalcode :parameter-type 'string) (st :parameter-type 'string) (l :parameter-type 'string) (telephonenumber :parameter-type 'string) (mobile :parameter-type 'string)) .inetorg-modify-submit)
(def-page :post "/inetorg/delete" ((cn :parameter-type 'string)) .inetorg-delete)
(def-page :post "/inetorg/delete/submit" ((cn :parameter-type 'string)) .inetorg-delete-submit)
(def-page :get "/inetorg/add" () .inetorg-add)
(def-page :post "/inetorg/add/submit" ((givenname :parameter-type 'string) (sn :parameter-type 'string) (mail :parameter-type 'string) (postaladdress :parameter-type 'string) (postalcode :parameter-type 'string) (st :parameter-type 'string) (l :parameter-type 'string) (telephonenumber :parameter-type 'string) (mobile :parameter-type 'string)) .inetorg-add-submit)