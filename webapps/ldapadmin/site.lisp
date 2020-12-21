;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:ldapadmin)

(defmacro .base (&optional (onload-fn "goto_location('/home')"))
  `(html5
    `(html
      (head
       ((meta :name "viewport" :content "width=device-width, initial-scale=1"))
       ((meta :charset "utf-8"))
       ((title) ,(title *webapp*))
       ,@(mapcar (lambda (css)
                   `((link :rel "stylesheet" :href ,(getf css :href) :integrity ,(getf css :integrity) :crossorigin ,(getf css :crossorigin))))
                 '((:href "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css" :integrity "sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm" :crossorigin "anonymous")))
       ,@(mapcar (lambda (js)
                   `((script :type "text/javascript" :src ,(getf js :src) :integrity ,(getf js :integrity) :crossorigin ,(getf js :crossorigin))))
                 '((:src "https://code.jquery.com/jquery-3.2.1.slim.min.js" :integrity "sha384-KJ3o2DKtIkvYIK3UENzmM7KCkRr/rE9/Qpg6aAZGJwFDMVNA/GpGFF93hXpG5KkN" :crossorigin "anonymous")
                   (:src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js" :integrity "sha384-ApNbgh9B+Y1QKtv3Rn7W3mgPxhU9K/ScQsAP7hUibX39j7fakFPskvXusvfa0b4Q" :crossorigin "anonymous")
                   (:src "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js" :integrity "sha384-JZR6Spejh4U02d8jOt6vLEHfe/JQGiRRSQQxSfFWpi1MquVdAyjUar5+76PVCmYl" :crossorigin "anonymous")))
       ((script :type "text/javascript" :src "/static/js/cljs/main.js")))
      ((body :onload ,(format nil "hworch.core.~a" ,onload-fn))
       ((div :class "container-fluid")
        ((div :class "row")
         ((div :class "col") "&nbsp;")
         ((div :class "col")
          ((div :class "page-header")
           ((h2 :align "center") ,(title *webapp*))))
         ((div :class "col") "&nbsp;"))
        ((div :id "menu" :class "well"))
        ((div :id "location" :style "display: none"))
        ((div :id "errormsg"))
        ((div :id "message"))
        ((div :id "body")))))))

(defmacro .location ()
  `(location-json location))

(defmacro .home-get ()
  `(home-json))

(defmacro .home-post ()
  `(home-json message errormsg))

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
  `(inetorg-view-results-json givenname sn mail postaladdress postalcode st l telephonenumber mobile businesscategory))

(defmacro .inetorg-modify ()
  `(inetorg-modify-json cn))

(defmacro .inetorg-modify-submit ()
  `(inetorg-modify-submit-json givenname sn mail postaladdress postalcode st l telephonenumber mobile businesscategory))

(defmacro .inetorg-delete ()
  `(inetorg-delete-json cn))

(defmacro .inetorg-delete-submit ()
  `(inetorg-delete-submit-json cn))

(defmacro .inetorg-add ()
  `(inetorg-add-json))

(defmacro .inetorg-add-submit ()
  `(inetorg-add-submit-json givenname sn mail postaladdress postalcode st l telephonenumber mobile businesscategory))

(define-endpoint :get "/" () .base)
(define-endpoint :post "/location" ((location :parameter-type 'string)) .location)
(define-endpoint :get "/home" () .home-get)
(define-endpoint :post "/home" ((message :parameter-type 'string) (errormsg :parameter-type 'string)) .home-post)
(define-endpoint :get "/menu" () .menu)
(define-endpoint :get "/login" () .login)
(define-endpoint :post "/login/authenticate" ((dn :parameter-type 'string) (password :parameter-type 'string)) .login-authenticate)
(define-endpoint :get "/logout" () .logout)
(define-endpoint :get "/inetorg/view" () .inetorg-view)
(define-endpoint :get "/inetorg/view/search" () .inetorg-view-search)
(define-endpoint :post "/inetorg/view/results" ((givenname :parameter-type 'string) (sn :parameter-type 'string) (mail :parameter-type 'string) (postaladdress :parameter-type 'string) (postalcode :parameter-type 'string) (st :parameter-type 'string) (l :parameter-type 'string) (telephonenumber :parameter-type 'string) (mobile :parameter-type 'string) (businesscategory :parameter-type 'string)) .inetorg-view-results)
(define-endpoint :post "/inetorg/modify" ((cn :parameter-type 'string)) .inetorg-modify)
(define-endpoint :post "/inetorg/modify/submit" ((givenname :parameter-type 'string) (sn :parameter-type 'string) (mail :parameter-type 'string) (postaladdress :parameter-type 'string) (postalcode :parameter-type 'string) (st :parameter-type 'string) (l :parameter-type 'string) (telephonenumber :parameter-type 'string) (mobile :parameter-type 'string) (businesscategory :parameter-type 'string)) .inetorg-modify-submit)
(define-endpoint :post "/inetorg/delete" ((cn :parameter-type 'string)) .inetorg-delete)
(define-endpoint :post "/inetorg/delete/submit" ((cn :parameter-type 'string)) .inetorg-delete-submit)
(define-endpoint :get "/inetorg/add" () .inetorg-add)
(define-endpoint :post "/inetorg/add/submit" ((givenname :parameter-type 'string) (sn :parameter-type 'string) (mail :parameter-type 'string) (postaladdress :parameter-type 'string) (postalcode :parameter-type 'string) (st :parameter-type 'string) (l :parameter-type 'string) (telephonenumber :parameter-type 'string) (mobile :parameter-type 'string) (businesscategory :parameter-type 'string)) .inetorg-add-submit)
