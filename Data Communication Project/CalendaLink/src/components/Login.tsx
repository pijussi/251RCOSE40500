import "./Login.css";
import { Link } from "react-router-dom";

function Login() {
  return (
    <div className="login-register">
      <div className="wrapper">
        <form action="">
          <h1>Login</h1>
          <div className="input-box">
            <input type="text" placeholder="Username" required></input>
          </div>
          <div className="input-box">
            <input type="password" placeholder="Password" required></input>
          </div>
          <div className="remember-forget">
            <label>
              <input type="checkbox" />
              Remember me
            </label>
          </div>
          <button type="submit">Login</button>
          <div className="register-link">
            <p>
              Don't have an account? <Link to="/Register">Register</Link>
            </p>
          </div>
          <Link to="/Home">Home</Link>
        </form>
      </div>
    </div>
  );
}

export default Login;
