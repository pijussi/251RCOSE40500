import { Link } from "react-router-dom";
import { IoSettingsOutline } from "react-icons/io5";
import { IoPersonSharp } from "react-icons/io5";
import "./ProfileBar.css";
import DisplayPicture from "../assets/DisplayProfile.jpg";

export const ProfileBar = () => {
  const personalProfile = "Syuhada";
  const phoneNumber = "01012345678";

  return (
    <nav className="profile-card">
      <ul>
        <Link to="/PersonalProfile">
          <li>
            <img
              src={DisplayPicture}
              alt="Profile Avatar"
              className="profile-avatar"
            />
            <div className="profile-details">
              <h3>{personalProfile}</h3>
              <p>{phoneNumber}</p>
            </div>
          </li>
        </Link>
        <div className="profile-settings">
          <Link to="/Setting">
            <IoSettingsOutline className="setting-icon" />
          </Link>
        </div>
      </ul>
    </nav>
  );
};
