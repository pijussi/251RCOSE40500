import React, { useState } from "react";
import "./CalendarsList.css";
import pic from "../assets/background.jpg";

interface EntryProps {
  calendarName: string;
  imageSrc: string;
  option1: () => void;
  option2: () => void;
}

const EntryFrame: React.FC<EntryProps> = ({
  calendarName,
  imageSrc,
  option1,
  option2,
}) => {
  const [isFavorite, setIsFavorite] = useState(false);
  const [isMenuOpen, setIsMenuOpen] = useState(false);

  const toggleFavorite = () => {
    setIsFavorite(!isFavorite);
  };

  const handleMenuToggle = () => {
    setIsMenuOpen((prev) => !prev); // Toggle the menu visibility
  };

  const handleMouseEnter = () => {
    setIsMenuOpen(true); // Show menu when mouse enters the button area
  };

  const handleMouseLeave = () => {
    setIsMenuOpen(false);
  };

  return (
    <div className="entry-frame">
      <div className="entry-title-container">
        <div
          className="more-vert-btn"
          onClick={handleMenuToggle}
          onMouseEnter={handleMouseEnter}
          onMouseLeave={handleMouseLeave}
        >
          <span className="material-icons">more_vert</span>
        </div>
        {isMenuOpen && (
          <div
            className="dropdown-menu"
            onMouseEnter={handleMouseEnter} // Keep menu open when mouse enters dropdown
            onMouseLeave={handleMouseLeave}
          >
            <button className="dropdown-item" onClick={option1}>
              Edit
            </button>
            <button className="dropdown-item" onClick={option2}>
              Delete
            </button>
          </div>
        )}
      </div>
      <div className="entry-image-container">
        <img className="entry-image" src={imageSrc} alt="Entry Image" />
      </div>
      <div className="entry-title-container">
        <div className="entry-calendarName">{calendarName}</div>
      </div>
      <div className="entry-buttons">
        <button
          className={`heart-btn ${isFavorite ? "favorite" : ""}`}
          onClick={toggleFavorite}
        >
          <span className="material-icons">
            {isFavorite ? "favorite" : "favorite_border"}
          </span>
        </button>
      </div>
    </div>
  );
};

const Entries: React.FC = () => {
  const entries = [
    { calendarName: "Calendar Name", imageSrc: pic },
    { calendarName: "Calendar Name", imageSrc: pic },
    { calendarName: "Calendar Name", imageSrc: pic },
    { calendarName: "Calendar Name", imageSrc: pic },
    { calendarName: "Calendar Name", imageSrc: pic },
    { calendarName: "Calendar Name", imageSrc: pic },
  ];

  const handleOption1 = (index: number) => {
    console.log(`option1 ${index}`);
  };

  const handleOption2 = (index: number) => {
    console.log(`option2 ${index}`);
  };

  return (
    <div className="entry-container">
      {entries.map((entry, index) => (
        <EntryFrame
          key={index}
          calendarName={entry.calendarName}
          imageSrc={entry.imageSrc}
          option1={() => handleOption1(index)}
          option2={() => handleOption2(index)}
        />
      ))}
    </div>
  );
};

export default Entries;
