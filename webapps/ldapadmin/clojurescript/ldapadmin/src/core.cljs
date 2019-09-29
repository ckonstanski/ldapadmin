(ns ldapadmin.core
  (:require-macros [hiccups.core :as hiccups :refer [html]])
  (:require [ajax.core :refer [GET POST]]
            [dommy.core :as dommy]
            [hiccups.runtime :as hiccupsrt]))

;; ========================================================================== ;;
;; declarations

(enable-console-print!)

(declare template-message)
(declare maybe-error)
(declare maybe-message)
(declare notifications)
(declare auth-notifications)
(declare template-generic-form)
(declare template-menu)
(declare handler-menu)
(declare render-menu)
(declare template-home)
(declare handler-home)
(declare render-home)
(declare handler-login)
(declare render-login)
(declare handler-login-authenticate)
(declare render-login-authenticate)
(declare handler-logout)
(declare render-logout)
(declare template-inetorg-view)
(declare handler-inetorg-view)
(declare render-inetorg-view)
(declare on-inetorg-view-search-clicked)
(declare handler-inetorg-view-search)
(declare render-inetorg-view-search)
(declare on-inetorg-modify-clicked)
(declare template-inetorg-view-results)
(declare handler-inetorg-view-results)
(declare render-inetorg-view-results)
(declare handler-inetorg-modify)
(declare render-inetorg-modify)
(declare template-inetorg-modify-submit)
(declare handler-inetorg-modify-submit)
(declare render-inetorg-modify-submit)
(declare on-inetorg-delete-clicked)
(declare template-inetorg-delete)
(declare handler-inetorg-delete)
(declare render-inetorg-delete)
(declare on-inetorg-delete-submit-clicked)
(declare template-inetorg-delete-submit)
(declare handler-inetorg-delete-submit)
(declare render-inetorg-delete-submit)
(declare handler-inetorg-add)
(declare render-inetorg-add)
(declare on-inetorg-add-submit-clicked)
(declare handler-inetorg-add-submit)
(declare render-inetorg-add-submit)
(declare template-location)
(declare on-menu-clicked)
(declare handler-location)
(declare goto-location)

;; ========================================================================== ;;
;; notifications

(hiccups/defhtml template-error [errormsg]
  [:div {:class "alert alert-danger"} errormsg])

(hiccups/defhtml template-message [message]
  [:div {:class "alert alert-success"} message])

(defn maybe-error [jsonobj]
  (cond (get jsonobj "errormsg")
        (dommy/set-html! (dommy/sel1 :#errormsg)
                         (template-error (get jsonobj "errormsg")))
        :else
        (dommy/set-html! (dommy/sel1 :#errormsg) "")))

(defn maybe-message [jsonobj]
  (cond (get jsonobj "message")
        (dommy/set-html! (dommy/sel1 :#message)
                         (template-message (get jsonobj "message")))
        :else
        (dommy/set-html! (dommy/sel1 :#message) "")))

(defn notifications [jsonobj]
  (maybe-error jsonobj)
  (maybe-message jsonobj))

(defn auth-notifications [jsonobj]
  (when (get jsonobj "errormsg")
    (render-home)
    (render-menu)))
  
;; ========================================================================== ;;
;; forms

(hiccups/defhtml template-generic-form
  ([jsonobj]
   (template-generic-form jsonobj "on_menu_clicked"))
  ([jsonobj onclick]
   [:form {:name (get jsonobj "name")
           :id (get jsonobj "name")
           :class "form-horizontal"
           :method (get jsonobj "httpMethod")}
    (for [form-field (get jsonobj "formFields")]
      (cond (= (get form-field "fieldType") "button")
            [:div {:class "col-sm-offset-2 col-sm-10"}
             [:button {:name (get form-field "name")
                       :id (get form-field "name")
                       :type (get form-field "fieldType")
                       :class "btn btn-primary"
                       :data-dismiss "modal"
                       :onclick (str (namespace ::x) "." onclick "('" (get jsonobj "action") "')")}
              (get form-field "label")]]
            :else
            [:div {:class "form-group"}
             [:label {:for (get form-field "name")
                      :class "control-label col-sm-2"}
              (get form-field "label")]
             [:div {:class "col-sm-10"}
              [:input {:name (get form-field "name")
                       :id (get form-field "name")
                       :type (get form-field "fieldType")
                       :class "form-control"}]]]))]))

;; ========================================================================== ;;
;; menu

(hiccups/defhtml template-menu [menuitems]
  [:div {:class "row"}
   (for [menuitem menuitems]
     [:div {:class "col-lg-3"}
      [:a {:class "menuitem"
           :id (get menuitem "id")
           :onclick (str (namespace ::x) ".on_menu_clicked('" (get menuitem "handler") "')")}
       (get menuitem "label")]])])

(defn handler-menu [response]
  (let [jsonobj (js->clj (js/JSON.parse response))]
    (dommy/set-html! (dommy/sel1 :#menu) (template-menu (get jsonobj "menuitems")))))

(defn render-menu []
  (GET "/menu" {:handler handler-menu}))

;; ========================================================================== ;;
;; home

(hiccups/defhtml template-home [jsonobj]
  [:h3 {:align "center"} (get jsonobj "content")])

(defn handler-home [response]
  (let [jsonobj (js->clj (js/JSON.parse response))]
    (dommy/set-html! (dommy/sel1 :#body) (template-home jsonobj))))

(defn render-home []
  (GET "/home" {:handler handler-home}))

;; ========================================================================== ;;
;; login

(defn handler-login [response]
  (let [jsonobj (js->clj (js/JSON.parse response))]
    (notifications jsonobj)
    (dommy/set-html! (dommy/sel1 :#body) (template-generic-form jsonobj))))

(defn render-login []
  (GET "/login" {:handler handler-login}))

;; ========================================================================== ;;
;; login-authenticate

(defn handler-login-authenticate [response]
  (let [jsonobj (js->clj (js/JSON.parse response))]
    (cond (get jsonobj "errormsg")
          (render-login)
          (get jsonobj "message")
          (render-home))
    (render-menu)
    (notifications jsonobj)))

(defn render-login-authenticate []
  (POST "/login/authenticate" {:format :raw
                               :params {:dn (dommy/value (dommy/sel1 :#dn))
                                        :password (dommy/value (dommy/sel1 :#password))}
                               :handler handler-login-authenticate}))

;; ========================================================================== ;;
;; logout

(defn handler-logout [response]
  (let [jsonobj (js->clj (js/JSON.parse response))]
    (render-home)
    (render-menu)
    (notifications jsonobj)))

(defn render-logout []
  (GET "/logout" {:handler handler-logout}))

;; ========================================================================== ;;
;; inetorg-view

(hiccups/defhtml template-inetorg-view [jsonobj]
  [:h3 {:align "center"} (get jsonobj "instructions")]
  [:div {:id "search"}]
  [:div {:id "results"}]
  [:div {:id "modify"
         :class "modal fade"
          :role "dialog"}
   [:div {:class "modal-dialog modal-lg"}
    [:div {:class "modal-content"}
     [:div {:class "modal-header"}
      [:button {:type "button"
                :class "close"
                :data-dismiss "modal"}
       "&times;"]
      [:h4 "Modify InetOrg Entry"]]
     [:div {:id "modify-body"
            :class "modal-body"
            :style "height: 510px;"}]
     [:div {:class "modal-footer"}
      [:button {:type "submit"
                :class "btn btn-danger btn-default"
                :data-dismiss "modal"}
       [:span {:class "glyphicon glyphicon-remove"}]
       "Cancel"]]]]]
    [:div {:id "delete"
         :class "modal fade"
          :role "dialog"}
   [:div {:class "modal-dialog"}
    [:div {:class "modal-content"}
     [:div {:class "modal-header"}
      [:button {:type "button"
                :class "close"
                :data-dismiss "modal"}
       "&times;"]
      [:h4 "Delete InetOrg Entry"]]
     [:div {:id "delete-body"
            :class "modal-body"}]
     [:div {:class "modal-footer"}
      [:button {:type "submit"
                :class "btn btn-danger btn-default"
                :data-dismiss "modal"}
       [:span {:class "glyphicon glyphicon-remove"}]
       "Cancel"]]]]])

(defn handler-inetorg-view [response]
  (let [jsonobj (js->clj (js/JSON.parse response))]
    (auth-notifications jsonobj)
    (dommy/set-html! (dommy/sel1 :#body) (template-inetorg-view jsonobj))
    (render-inetorg-view-search)))

(defn render-inetorg-view []
  (GET "/inetorg/view" {:handler handler-inetorg-view}))

;; ========================================================================== ;;
;; inetorg-view-search

(defn on-inetorg-view-search-clicked [handler]
  (render-inetorg-view-results))

(defn handler-inetorg-view-search [response]
  (let [jsonobj (js->clj (js/JSON.parse response))]
    (auth-notifications jsonobj)
    (dommy/set-html! (dommy/sel1 :#search) (template-generic-form jsonobj "on_inetorg_view_search_clicked"))
    (render-inetorg-view-results)))

(defn render-inetorg-view-search []
  (GET "/inetorg/view/search" {:handler handler-inetorg-view-search}))

;; ========================================================================== ;;
;; inetorg-view-results

(hiccups/defhtml template-inetorg-view-results [jsonobj]
  [:table {:class "table table-hover"}
   [:thead
    [:tr
     [:th "givenname"]
     [:th "sn"]
     [:th "mail"]
     [:th "postaladdress"]
     [:th "postalcode"]
     [:th "st"]
     [:th "l"]
     [:th "telephoneNumber"]
     [:th "mobile"]
     [:th "businesscategory"]
     [:th "Del"]]]
   [:tbody
    (for [ldap-user (get jsonobj "results")]
      (let* [cn (str (get ldap-user "givenname") " " (get ldap-user "sn"))
             onclick (str (namespace ::x) ".on_inetorg_modify_clicked('" cn "')")]
        [:tr
         [:td {:onclick onclick} (get ldap-user "givenname")]
         [:td {:onclick onclick} (get ldap-user "sn")]
         [:td {:onclick onclick} (get ldap-user "mail")]
         [:td {:onclick onclick} (get ldap-user "postaladdress")]
         [:td {:onclick onclick} (get ldap-user "postalcode")]
         [:td {:onclick onclick} (get ldap-user "st")]
         [:td {:onclick onclick} (get ldap-user "l")]
         [:td {:onclick onclick} (get ldap-user "telephonenumber")]
         [:td {:onclick onclick} (get ldap-user "mobile")]
         [:td {:onclick onclick} (get ldap-user "businesscategory")]
         [:td [:img {:src "/static/images/edit-delete.png"
                     :onclick (str (namespace ::x) ".on_inetorg_delete_clicked('" cn "')")}]]]))]])

(defn handler-inetorg-view-results [response]
  (let [jsonobj (js->clj (js/JSON.parse response))]
    (auth-notifications jsonobj)
    (dommy/set-html! (dommy/sel1 :#results) (template-inetorg-view-results jsonobj))))

(defn render-inetorg-view-results []
  (POST "/inetorg/view/results" {:format :raw
                                 :params {:givenname (dommy/value (dommy/sel1 :#view-givenname))
                                          :sn (dommy/value (dommy/sel1 :#view-sn))
                                          :mail (dommy/value (dommy/sel1 :#view-mail))
                                          :postaladdress (dommy/value (dommy/sel1 :#view-postaladdress))
                                          :postalcode (dommy/value (dommy/sel1 :#view-postalcode))
                                          :st (dommy/value (dommy/sel1 :#view-st))
                                          :l (dommy/value (dommy/sel1 :#view-l))
                                          :telephonenumber (dommy/value (dommy/sel1 :#view-telephonenumber))
                                          :mobile (dommy/value (dommy/sel1 :#view-mobile))
                                          :businesscategory (dommy/value (dommy/sel1 :#view-businesscategory))}
                                 :handler handler-inetorg-view-results}))

;; ========================================================================== ;;
;; inetorg-modify

(defn on-inetorg-modify-clicked [cn]
  (render-inetorg-modify cn))

(defn handler-inetorg-modify [response]
  (let [jsonobj (js->clj (js/JSON.parse response))
        jquery (js* "$")]
    (auth-notifications jsonobj)
    (dommy/set-html! (dommy/sel1 :#modify-body) (template-generic-form jsonobj "on_inetorg_modify_submit_clicked"))
    (doseq [[name value] (get jsonobj "ldapUserValues")]
      (dommy/set-value! (dommy/sel1 (keyword (str "#modify-" name))) value))
    (.modal (jquery "#modify"))))

(defn render-inetorg-modify [cn]
  (POST "/inetorg/modify" {:format :raw
                           :params {:cn cn}
                           :handler handler-inetorg-modify}))

;; ========================================================================== ;;
;; inetorg-modify-submit

(defn on-inetorg-modify-submit-clicked []
  (render-inetorg-modify-submit))

(defn handler-inetorg-modify-submit [response]
  (let [jsonobj (js->clj (js/JSON.parse response))]
    (auth-notifications jsonobj)
    (notifications jsonobj)
    (on-menu-clicked "/inetorg/view")))

(defn render-inetorg-modify-submit []
  (POST "/inetorg/modify/submit" {:format :raw
                                  :params {:givenname (dommy/value (dommy/sel1 :#modify-givenname))
                                           :sn (dommy/value (dommy/sel1 :#modify-sn))
                                           :mail (dommy/value (dommy/sel1 :#modify-mail))
                                           :postaladdress (dommy/value (dommy/sel1 :#modify-postaladdress))
                                           :postalcode (dommy/value (dommy/sel1 :#modify-postalcode))
                                           :st (dommy/value (dommy/sel1 :#modify-st))
                                           :l (dommy/value (dommy/sel1 :#modify-l))
                                           :telephonenumber (dommy/value (dommy/sel1 :#modify-telephonenumber))
                                           :mobile (dommy/value (dommy/sel1 :#modify-mobile))
                                           :businesscategory (dommy/value (dommy/sel1 :#modify-businesscategory))}
                                  :handler handler-inetorg-modify-submit}))

;; ========================================================================== ;;
;; inetrog-delete

(defn on-inetorg-delete-clicked [cn]
  (render-inetorg-delete cn))

(hiccups/defhtml template-inetorg-delete [jsonobj]
  [:p (str "Are you sure you want to delete the InetOrg entry: " (get jsonobj "cn"))]
  [:p "This action cannot be undone."]
  [:button {:type "button"
            :data-dismiss "modal"
            :onclick (str (namespace ::x) ".on_inetorg_delete_submit_clicked('" (get jsonobj "cn") "')")}
   "Delete InetOrg Entry"])

(defn handler-inetorg-delete [response]
  (let [jsonobj (js->clj (js/JSON.parse response))
        jquery (js* "$")]
    (auth-notifications jsonobj)
    (dommy/set-html! (dommy/sel1 :#delete-body) (template-inetorg-delete jsonobj))
    (.modal (jquery "#delete"))))

(defn render-inetorg-delete [cn]
  (POST "/inetorg/delete" {:format :raw
                           :params {:cn cn}
                           :handler handler-inetorg-delete}))

;; ========================================================================== ;;
;; inetrog-delete-submit

(defn on-inetorg-delete-submit-clicked [cn]
  (render-inetorg-delete-submit cn))

(defn handler-inetorg-delete-submit [response]
  (let [jsonobj (js->clj (js/JSON.parse response))]
    (auth-notifications jsonobj)
    (notifications jsonobj)
    (on-menu-clicked "/inetorg/view")))

(defn render-inetorg-delete-submit [cn]
  (POST "/inetorg/delete/submit" {:format :raw
                                  :params {:cn cn}
                                  :handler handler-inetorg-delete-submit}))

;; ========================================================================== ;;
;; inetrog-add

(defn handler-inetorg-add [response]
  (let [jsonobj (js->clj (js/JSON.parse response))]
    (auth-notifications jsonobj)
    (dommy/set-html! (dommy/sel1 :#body) (template-generic-form jsonobj "on_inetorg_add_submit_clicked"))))

(defn render-inetorg-add []
  (GET "/inetorg/add" {:handler handler-inetorg-add}))

;; ========================================================================== ;;
;; inetorg-add-submit

(defn on-inetorg-add-submit-clicked [handler]
  (render-inetorg-add-submit))

(defn handler-inetorg-add-submit [response]
  (let [jsonobj (js->clj (js/JSON.parse response))]
    (auth-notifications jsonobj)
    (notifications jsonobj)
    (on-menu-clicked "/inetorg/view")))

(defn render-inetorg-add-submit []
  (POST "/inetorg/add/submit" {:format :raw
                               :params {:givenname (dommy/value (dommy/sel1 :#add-givenname))
                                        :sn (dommy/value (dommy/sel1 :#add-sn))
                                        :mail (dommy/value (dommy/sel1 :#add-mail))
                                        :postaladdress (dommy/value (dommy/sel1 :#add-postaladdress))
                                        :postalcode (dommy/value (dommy/sel1 :#add-postalcode))
                                        :st (dommy/value (dommy/sel1 :#add-st))
                                        :l (dommy/value (dommy/sel1 :#add-l))
                                        :telephonenumber (dommy/value (dommy/sel1 :#add-telephonenumber))
                                        :mobile (dommy/value (dommy/sel1 :#add-mobile))
                                        :businesscategory (dommy/value (dommy/sel1 :#add-businesscategory))}
                               :handler handler-inetorg-add-submit}))

;; ========================================================================== ;;
;; location

(hiccups/defhtml template-location [location]
  [:h3 {:align "center"} location])

(defn on-menu-clicked [handler]
   (dommy/set-html! (dommy/sel1 :#location) (clojure.string/upper-case (template-location handler)))
   (cond (= handler "/home") (render-home)
         (= handler "/login") (render-login)
         (= handler "/login/authenticate") (render-login-authenticate)
         (= handler "/logout") (render-logout)
         (= handler "/inetorg/view") (render-inetorg-view)
         (= handler "/inetorg/add") (render-inetorg-add)))

(defn handler-location [response]
  (let [jsonobj (js->clj (js/JSON.parse response))]
    (on-menu-clicked (get jsonobj "location"))
    (render-menu)
    (notifications jsonobj)))

(defn goto-location []
  (GET "/location" {:handler handler-location}))

(set! (.-onload js/window) goto-location)
