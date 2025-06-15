import { Link } from "react-router-dom";
import BotNavBar from "../BottomNavBar/BotNavBar";
import { NavBar } from "./NavBar";
import "./PersonalProfile.css";
import ProfileBackground from "../assets/ProfileBackground.jpg";
import DisplayPicture from "../assets/DisplayProfile.jpg";

function PersonalProfile() {
  return (
    <div>
      <NavBar />
      <div className="personal-profile-box">
        <div className="user-profile">
          {/* Background and Profile Picture */}
          <div className="profile-header">
            <img
              src={ProfileBackground}
              alt="Background"
              className="background-image"
            />
          </div>

          <div className="profile-picture">
            <img src={DisplayPicture} alt="Profile" />
          </div>

          {/* Name and Basic Information */}
          <div className="profile-info">
            <h2>Syuhada</h2>
            <div className="info-field">
              <span className="info-label">Name:</span>
              <span className="info-value">Syuhada</span>
            </div>
            <div className="info-field">
              <span className="info-label">Email Address:</span>
              <span className="info-value">syushou@gmail.com</span>
            </div>
            <div className="info-field">
              <span className="info-label">Single ID:</span>
              <span className="info-value">syushou</span>
            </div>

            {/* Logout Button */}
            <div className="logout-container">
              <Link to="/" className="logout-button">
                Logout
              </Link>
            </div>
          </div>
        </div>
      </div>
      <BotNavBar />
    </div>
  );
}

export default PersonalProfile;
