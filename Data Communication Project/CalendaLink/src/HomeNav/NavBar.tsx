import { Link } from "react-router-dom";
import { IoIosNotifications } from "react-icons/io";

import "./NavBar.css";

export const NavBar = () => {
  return (
    <nav className="navbar">
      <ul className="left-upper-navbar">
        <Link to="/Home">Home</Link>
      </ul>
      <ul className="right-upper-navbar">
        <Link to="/Notification">
          <IoIosNotifications />
        </Link>
        <Link to="/AllCalendar">All Calendar</Link>
      </ul>
    </nav>
  );
};
