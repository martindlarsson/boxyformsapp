module Event.Models exposing(Event, EventForm)

type alias Event =
    { eventId : Int
    , eventName : String
    , eventForms : (List EventForm)
    , organizer : String
    , organizerId : Int
    , public : Bool
    , webLink : String
    }

type alias EventForm =
    { formId : Int
    , formName : String
    }