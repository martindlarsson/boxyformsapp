module Event.Models exposing(Event)

type alias Event =
    { eventId : Int
    , eventName : String
    , formId : Int -- TODO, list of forms
    , organizer : String
    , organizerId : Int
    , public : Bool
    , webLink : String
    }