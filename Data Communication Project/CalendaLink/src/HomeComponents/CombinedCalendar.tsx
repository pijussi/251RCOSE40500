import BotNavBar from "../BottomNavBar/BotNavBar";
import CalendarDisplay from "../Calendar/CalendarDisplay";
import EventSideBar from "../Calendar/EventSideBar";
import { NavBar } from "../HomeNav/NavBar";

function CombinedCalendar() {
  return (
    <div>
      <NavBar />
      <div style={{ display: "flex" }}>
        <EventSideBar />
        <CalendarDisplay />
      </div>
      <BotNavBar />
    </div>
  );
}

export default CombinedCalendar;
