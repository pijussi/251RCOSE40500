import React, { useState } from "react";
import { NavBar } from "./HomeNav/NavBar";
import FriendList from "./HomeComponents/FriendList";
import BotNavBar from "./BottomNavBar/BotNavBar";
import CalendarDisplay from "./Calendar/CalendarDisplay";
import { ProfileBar } from "./HomeNav/ProfileBar";
import "./Home.css";
import EventPopup from "./HomeComponents/EventPopup";
import DeleteButton from "./components/DeleteButton";

function Home() {
  const [isEventPopupOpen, setIsEventPopupOpen] = useState(false);

  return (
    <div>
      <NavBar />
      <div className="main-container">
        <ul>
          <ProfileBar />
          <FriendList />
        </ul>
        <CalendarDisplay />
      </div>
      <BotNavBar />

      <DeleteButton />

      <button
        className="add-event-button"
        onClick={() => setIsEventPopupOpen(true)}
      >
        +
      </button>

      {isEventPopupOpen && (
        <EventPopup onClose={() => setIsEventPopupOpen(false)} />
      )}
    </div>
  );
}

export default Home;
