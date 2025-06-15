import { Link } from "react-router-dom";
import "./BotNavBar.css";
import CalendaLink from "../assets/CalendaLink.png";

function BotNavBar() {
  return (
    <div className="bot-nav-bar">
      <Link to="/About">
        <img src={CalendaLink} alt=""></img>
      </Link>
    </div>
  );
}

export default BotNavBar;
