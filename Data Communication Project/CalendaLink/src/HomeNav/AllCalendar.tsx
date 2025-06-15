import BotNavBar from "../BottomNavBar/BotNavBar";
import CalendarsList from "./CalendarsList";
import { NavBar } from "./NavBar";

function AllCalendar() {
  return (
    <div>
      <NavBar />
      <h1>All Calendar</h1>
      <CalendarsList />
      <BotNavBar />
    </div>
  );
}

export default AllCalendar;
