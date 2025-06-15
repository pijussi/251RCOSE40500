import Register from "./components/Register";
import Login from "./components/Login";
import { Routes, Route } from "react-router-dom";
import Home from "./Home";
import Notification from "./HomeNav/Notification";
import AllCalendar from "./HomeNav/AllCalendar";
import PersonalProfile from "./HomeNav/PersonalProfile";
import CombinedCalendar from "./HomeComponents/CombinedCalendar";
import Setting from "./HomeNav/Setting";
import About from "./BottomNavBar/About";

function App() {
  return (
    <div>
      <Routes>
        <Route path="Register" element={<Register />}></Route>
        <Route path="/" element={<Login />}></Route>
        <Route path="Home" element={<Home />}></Route>
        <Route path="Notification" element={<Notification />}></Route>
        <Route path="AllCalendar" element={<AllCalendar />}></Route>
        <Route path="PersonalProfile" element={<PersonalProfile />}></Route>
        <Route path="CombinedCalendar" element={<CombinedCalendar />}></Route>
        <Route path="Setting" element={<Setting />}></Route>
        <Route path="About" element={<About />}></Route>
      </Routes>
    </div>
  );
}

export default App;
