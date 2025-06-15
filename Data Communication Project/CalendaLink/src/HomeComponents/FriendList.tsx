import { IoAdd } from "react-icons/io5";
import { useState } from "react";
import { Link } from "react-router-dom";
import "./FriendList.css";
import { IoChevronForward } from "react-icons/io5";
import ContactSelector from "./ContactSelector";

function FriendList() {
  const [isContactSelectorOpen, setIsContactSelectorOpen] = useState(false);

  return (
    <div className="friend-list-container">
      <div className="link-container">
        <Link to="/CombinedCalendar" className="calendar-button">
          Combined Calendar
          <IoChevronForward className="arrow-icon" />
        </Link>
      </div>
      <ul className="friend-list">
        <li>
          <div className="avatar">
            <span>A</span>
            <div>
              <p className="friend-name">Afiqah</p>
              <p className="friend-id">010-12345678</p>
            </div>
          </div>
        </li>
        <li>
          <div className="avatar">
            <span>A</span>
            <div>
              <p className="friend-name">Anis</p>
              <p className="friend-id">010-12345678</p>
            </div>
          </div>
        </li>
        <li>
          <div className="avatar">
            <span>F</span>
            <div>
              <p className="friend-name">Faiz</p>
              <p className="friend-id">010-12345678</p>
            </div>
          </div>
        </li>
        <li>
          <div className="avatar">
            <span>F</span>
            <div>
              <p className="friend-name">Fizu</p>
              <p className="friend-id">010-12345678</p>
            </div>
          </div>
        </li>
        <li className="all-item">
          <div className="avatar">
            <span>A</span>
            <div>
              <p className="friend-name">All</p>
            </div>
          </div>
        </li>
      </ul>
      <button
        className="add-button"
        onClick={() => setIsContactSelectorOpen(true)}
      >
        <IoAdd /> Add
      </button>

      {isContactSelectorOpen && (
        <div className="popup-overlay">
          <ContactSelector onClose={() => setIsContactSelectorOpen(false)} />
        </div>
      )}
    </div>
  );
}

export default FriendList;
