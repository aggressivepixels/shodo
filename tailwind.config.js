/** @type {import('tailwindcss').Config} */

module.exports = {
  content: ["./src/**/*.elm", "index.html"],
  theme: {
    extend: {
      fontFamily: {
        epkyouka: ["EPKYOUKA", "sans-serif"],
      },
    },
  },
  plugins: [require("@tailwindcss/forms")],
};
