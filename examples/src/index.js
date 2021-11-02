import "./Storybook.css";

if (process.env.NODE_ENV === "production") {
  require("bundle");
} else {
  require("Main").main();
}
