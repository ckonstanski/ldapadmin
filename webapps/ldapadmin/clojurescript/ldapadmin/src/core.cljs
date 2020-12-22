(ns ldapadmin.core
  (:require-macros [hiccups.core :as hiccups :refer [html]])
  (:require [ajax.core :refer [GET POST]]
            [dommy.core :as dommy]
            [hiccups.runtime :as hiccupsrt]
            [clojure.string :as str]))

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
(declare template-login)
(declare handler-login)
(declare render-login)
(declare on-login-submit-clicked)
(declare handler-login-authenticate)
(declare render-login-authenticate)
(declare handler-logout)
(declare render-logout)
(declare template-inetorg-view)
(declare handler-inetorg-view)
(declare render-inetorg-view)
(declare on-inetorg-view-search-clicked)
(declare template-inetorg-view-search)
(declare handler-inetorg-view-search)
(declare render-inetorg-view-search)
(declare template-inetorg-view-results)
(declare handler-inetorg-view-results)
(declare render-inetorg-view-results)
(declare on-inetorg-modify-clicked)
(declare template-inetorg-modify)
(declare handler-inetorg-modify)
(declare render-inetorg-modify)
(declare on-inetorg-modify-submit-clicked)
(declare handler-inetorg-modify-submit)
(declare render-inetorg-modify-submit)
(declare on-inetorg-delete-clicked)
(declare template-inetorg-delete)
(declare handler-inetorg-delete)
(declare render-inetorg-delete)
(declare on-inetorg-delete-submit-clicked)
(declare handler-inetorg-delete-submit)
(declare render-inetorg-delete-submit)
(declare template-inetorg-add)
(declare handler-inetorg-add)
(declare render-inetorg-add)
(declare on-inetorg-add-submit-clicked)
(declare handler-inetorg-add-submit)
(declare render-inetorg-add-submit)
(declare on-menu-clicked)
(declare handler-location)
(declare goto-location)

(def jquery (js* "$"))

;; notifications

(hiccups/defhtml template-error [errormsg]
  [:div {:class "alert alert-danger"} errormsg])

(hiccups/defhtml template-message [message]
  [:div {:class "alert alert-success"} message])

(defn maybe-error [jsonobj]
  (let [errormsg (get jsonobj "errormsg")]
    (cond (or (= nil errormsg) (= "" errormsg))
          (dommy/set-style! (dommy/sel1 :#errormsg) :display "none")
          :else
          (do
            (dommy/set-style! (dommy/sel1 :#errormsg) :display "block")
            (dommy/set-html! (dommy/sel1 :#errormsg)
                             (template-error errormsg))))))

(defn maybe-message [jsonobj]
  (let [message (get jsonobj "message")]
    (cond (or (= nil message) (= "" message))
          (dommy/set-style! (dommy/sel1 :#message) :display "none")
          :else
          (do
            (dommy/set-style! (dommy/sel1 :#message) :display "block")
            (dommy/set-html! (dommy/sel1 :#message)
                             (template-message message))))))

(defn notifications [jsonobj]
  (maybe-error jsonobj)
  (maybe-message jsonobj))

(defn auth-notifications [jsonobj]
  (cond (get jsonobj "errormsg")
        (do
          (render-home "" (get jsonobj "errormsg"))
          (render-menu))
        :else
        (maybe-message jsonobj)))
  
;; forms

(hiccups/defhtml template-generic-form [jsonobj]
   [:div {:id "form-errormsg"}]
   [:form {:name (get jsonobj "name")
           :id (get jsonobj "name")
           :method (get jsonobj "httpMethod")
           :action (when (get jsonobj "action")
                     (str "javascript:" (namespace ::x) "." (get jsonobj "action")))}
    (for [form-field (get jsonobj "formFields")]
      (cond (= (get form-field "fieldType") "button")
            [:div {:class "form-group row"}
             [:div {:class "offset-sm-2 col-sm-10"}
              [:button {:class "btn btn-primary"
                        :data-dismiss (get jsonobj "dismiss")
                        :type (cond (get form-field "onclick") "button" :else "submit")
                        :onclick (when (get form-field "onclick")
                                   (str (namespace ::x) "." (get form-field "onclick")))}
               (get form-field "label")]]]
            (= (get form-field "fieldType") "hidden")
            [:input {:name (get form-field "name")
                     :id (get form-field "name")
                     :value (get form-field "value")
                     :type (get form-field "fieldType")}]
            (= (get form-field "fieldType") "checkbox")
            [:div {:class "form-check"}
             [:input {:type "checkbox"
                      :class "form-check-input"
                      :id (get form-field "name")
                      :value (get form-field "value")
                      :checked (get form-field "checked")
                      :required (get form-field "required")}]
             (when (get form-field "label")
               [:label {:class "form-check-label"
                        :for (get form-field "name")}
                (get form-field "label")])]
            (= (get form-field "fieldType") "select")
            [:div {:class "form-group"}
             [:label {:for (get form-field "name")}
              (get form-field "label")]
             [:select {:class "form-control"
                       :name (get form-field "name")
                       :id (get form-field "name")
                       :required (get form-field "required")
                       :onchange (when (get form-field "onchange")
                                   (str (namespace ::x) "." (get form-field "onchange")))}
              (for [option (get form-field "options")]
                [:option {:value (get option "value")
                          :selected (when (= (get option "value") (get form-field "value"))
                                      "selected")}
                 (get option "label")])]]
            (= (get form-field "fieldType") "textarea")
            [:div {:class "form-group row"}
             (when (get form-field "label")
               [:label {:for (get form-field "name")
                        :class "col-form-label col-sm-2"
                        :style "text-align: right"}
                (get form-field "label")])
             [:div {:class "col-sm-10"}
              [:textarea {:name (get form-field "name")
                          :id (get form-field "name")
                          :rows "10"
                          :cols "68"}
               (get form-field "value")]]]
            (= (get form-field "fieldType") "datetime-local")
            [:div {:class "form-group row"}
             (when (get form-field "label")
               [:label {:for (get form-field "name")
                        :class "col-form-label col-sm-2"
                        :style "text-align: right"}
                (get form-field "label")])
             [:div {:class "col-sm-10"}
              [:input {:name (get form-field "name")
                       :id (get form-field "name")
                       :value (get form-field "value")
                       :type (get form-field "fieldType")
                       :class "form-control"
                       :required (get form-field "required")
                       :min "2018-01-01T00:00"
                       :max "2020:12-31T23:59"}]]]
            :else
            [:div {:class "form-group row"}
             (when (get form-field "label")
               [:label {:for (get form-field "name")
                        :class "col-form-label col-sm-2"
                        :style "text-align: right"}
                (get form-field "label")])
             [:div {:class "col-sm-10"}
              [:input {:name (get form-field "name")
                       :id (get form-field "name")
                       :value (get form-field "value")
                       :type (get form-field "fieldType")
                       :class "form-control"
                       :required (get form-field "required")}]]]))]
  (when (get jsonobj "requiredP")
    [:div "* Required"]))

;; menu

(hiccups/defhtml template-menu [menuitems]
  [:ul {:class "nav nav-pills"}
   (for [menuitem menuitems]
     [:li {:class "nav-item"}
      [:a {:class (cond (= (str/upper-case (get menuitem "handler"))
                           (str/upper-case (dommy/html (dommy/sel1 :#location))))
                        "nav-link active"
                        :else
                        "nav-link")
           :id (get menuitem "id")
           :onclick (str (namespace ::x) ".on_menu_clicked('" (get menuitem "handler") "')")}
       (get menuitem "label")]])])

(defn handler-menu [response]
  (let [jsonobj (js->clj (js/JSON.parse response))]
    (dommy/set-html! (dommy/sel1 :#menu) (template-menu (get jsonobj "menuitems")))))

(defn render-menu []
  (GET "/menu" {:handler handler-menu}))

;; home

(hiccups/defhtml template-home [jsonobj]
  [:h3 {:style "text-align: center"} (get jsonobj "content")])

(defn handler-home [response]
  (let [jsonobj (js->clj (js/JSON.parse response))]
    (notifications jsonobj)
    (dommy/set-html! (dommy/sel1 :#body) (template-home jsonobj))))

(defn render-home
  ([]
   (GET "/home" {:handler handler-home}))
  ([message errormsg]
   (POST  "/home" {:format :raw
                   :params {:message message
                            :errormsg errormsg}
                   :handler handler-home})))

;; login

(hiccups/defhtml template-login [jsonobj]
  [:h3 {:style "text-align: center"} (get jsonobj "title")]
  (template-generic-form (get jsonobj "form"))
  [:div {:style "text-align: center"}])

(defn handler-login [response]
  (let [jsonobj (js->clj (js/JSON.parse response))]
    (notifications jsonobj)
    (dommy/set-html! (dommy/sel1 :#body) (template-login jsonobj))))

(defn render-login []
  (GET "/login" {:handler handler-login}))

;; login-authenticate

(defn on-login-submit-clicked []
  (when (-> (jquery "#login-form")
            (.get "0")
            (.checkValidity))
    (render-login-authenticate)))

(defn handler-login-authenticate [response]
  (let [jsonobj (js->clj (js/JSON.parse response))]
    (cond (get jsonobj "errormsg")
          (do
            (notifications jsonobj)
            (render-login))
          (get jsonobj "message")
          (do
            (dommy/set-html! (dommy/sel1 :#location) "/home")
            (render-home (get jsonobj "message") "")))
    (render-menu)))

(defn render-login-authenticate []
  (POST "/login/authenticate" {:format :raw
                               :params {:dn (dommy/value (dommy/sel1 :#dn))
                                        :password (dommy/value (dommy/sel1 :#password))}
                               :handler handler-login-authenticate}))

;; logout

(defn handler-logout [response]
  (let [jsonobj (js->clj (js/JSON.parse response))]
    (render-home"You are now logged out" "")
    (dommy/set-html! (dommy/sel1 :#location) "/home")
    (render-menu)))

(defn render-logout []
  (GET "/logout" {:handler handler-logout}))

;; inetorg-view

(hiccups/defhtml template-inetorg-view [jsonobj]
  [:h3 {:style "text-align: center"} (get jsonobj "instructions")]
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

;; inetorg-view-search

(defn on-inetorg-view-search-clicked []
  (when (-> (jquery "#inetorg-view-search-form")
            (.get "0")
            (.checkValidity))
    (render-inetorg-view-results)))

(hiccups/defhtml template-inetorg-view-search [jsonobj]
  (template-generic-form (get jsonobj "form")))

(defn handler-inetorg-view-search [response]
  (let [jsonobj (js->clj (js/JSON.parse response))]
    (auth-notifications jsonobj)
    (dommy/set-html! (dommy/sel1 :#search) (template-inetorg-view-search jsonobj))
    (render-inetorg-view-results)))

(defn render-inetorg-view-search []
  (GET "/inetorg/view/search" {:handler handler-inetorg-view-search}))

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

;; inetorg-modify

(defn on-inetorg-modify-clicked [cn]
  (render-inetorg-modify cn))

(hiccups/defhtml template-inetorg-modify [jsonobj]
  (template-generic-form (get jsonobj "form")))

(defn handler-inetorg-modify [response]
  (let [jsonobj (js->clj (js/JSON.parse response))
        jquery (js* "$")]
    (auth-notifications jsonobj)
    (dommy/set-html! (dommy/sel1 :#modify-body) (template-inetorg-modify jsonobj))
    (doseq [[name value] (get jsonobj "ldapUserValues")]
      (dommy/set-value! (dommy/sel1 (keyword (str "#modify-" name))) value))
    (.modal (jquery "#modify"))))

(defn render-inetorg-modify [cn]
  (POST "/inetorg/modify" {:format :raw
                           :params {:cn cn}
                           :handler handler-inetorg-modify}))

;; inetorg-modify-submit

(defn on-inetorg-modify-submit-clicked []
  (.modal (jquery "#modify") "hide")
  (render-inetorg-modify-submit))

(defn handler-inetorg-modify-submit [response]
  (let [jsonobj (js->clj (js/JSON.parse response))]
    (auth-notifications jsonobj)
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

;; inetrog-delete-submit

(defn on-inetorg-delete-submit-clicked [cn]
  (render-inetorg-delete-submit cn))

(defn handler-inetorg-delete-submit [response]
  (let [jsonobj (js->clj (js/JSON.parse response))]
    (auth-notifications jsonobj)
    (on-menu-clicked "/inetorg/view")))

(defn render-inetorg-delete-submit [cn]
  (POST "/inetorg/delete/submit" {:format :raw
                                  :params {:cn cn}
                                  :handler handler-inetorg-delete-submit}))

;; inetrog-add

(hiccups/defhtml template-inetorg-add [jsonobj]
  (template-generic-form (get jsonobj "form")))

(defn handler-inetorg-add [response]
  (let [jsonobj (js->clj (js/JSON.parse response))]
    (auth-notifications jsonobj)
    (dommy/set-html! (dommy/sel1 :#body) (template-inetorg-add jsonobj))))

(defn render-inetorg-add []
  (GET "/inetorg/add" {:handler handler-inetorg-add}))

;; inetorg-add-submit

(defn on-inetorg-add-submit-clicked []
  (when (-> (jquery "#inetorg-add-form")
            (.get "0")
            (.checkValidity))
    (render-inetorg-add-submit)))

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

;; location

(defn on-menu-clicked [handler]
  (dommy/set-html! (dommy/sel1 :#location) handler)
  (render-menu)
  (cond (= handler "/home") (render-home)
        (= handler "/login") (render-login)
        (= handler "/login/authenticate") (render-login-authenticate)
        (= handler "/logout") (render-logout)
        (= handler "/inetorg/view") (render-inetorg-view)
        (= handler "/inetorg/add") (render-inetorg-add)))

(defn handler-location [response]
  (let [jsonobj (js->clj (js/JSON.parse response))]
    (on-menu-clicked (get jsonobj "location"))
    (notifications jsonobj)))

(defn goto-location [location]
  (POST "/location" {:format :raw
                     :params {:location location}
                     :handler handler-location}))
