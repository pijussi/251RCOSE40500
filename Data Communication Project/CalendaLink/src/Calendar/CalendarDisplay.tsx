import { ScheduleXCalendar, useCalendarApp } from "@schedule-x/react";
import { createViewWeek, createViewMonthGrid } from "@schedule-x/calendar";
import "@schedule-x/theme-default/dist/index.css";
import { createEventModalPlugin } from "@schedule-x/event-modal";
import { createDragAndDropPlugin } from "@schedule-x/drag-and-drop";

function CalendarDisplay() {
  const calendar = useCalendarApp({
    views: [createViewWeek(), createViewMonthGrid()],
    events: [
      {
        id: 1,
        title: "Final Presentation",
        start: "2024-12-09 13:30",
        end: "2024-12-09 14:45",
        description: "Bonjour, Je Mapeile...",
      },
      {
        id: 2,
        title: "Main fon time Present",
        start: "2024-12-09 13:30",
        end: "2024-12-09 14:45",
        description: "Dah A+ dah",
        calendarId: "leisure",
      },
      {
        id: 3,
        title: "Makan after Kelas",
        start: "2024-12-09 14:45",
        end: "2024-12-09 16:45",
        description: "makan setelah habis kelas",
        calendarId: "important",
      },
      {
        id: 4,
        title: "Minum after kelas",
        start: "2024-12-09 15:45",
        end: "2024-12-09 16:45",
        description: "Minum luuu",
        calendarId: "leisure",
      },
      {
        id: 5,
        title: "Second Round",
        start: "2024-12-09 16:20",
        end: "2024-12-09 20:45",
        description: "Second round after kelas",
        calendarId: "lessimportant",
      },
    ],
    calendars: {
      leisure: {
        colorName: "leisure",
        lightColors: {
          main: "#1c7df9",
          container: "#d2e7ff",
          onContainer: "#002859",
        },
        darkColors: {
          main: "#c0dfff",
          onContainer: "#dee6ff",
          container: "#426aa2",
        },
      },
      important: {
        colorName: "important",
        lightColors: {
          main: "#f91c1c",
          container: "#ffd2d2",
          onContainer: "#590000",
        },
        darkColors: {
          main: "#ffc0c0",
          onContainer: "#ffdede",
          container: "#a24242",
        },
      },
      lessimportant: {
        colorName: "lessimportant",
        lightColors: {
          main: "#dcf91c",
          container: "#f8ffd2",
          onContainer: "#595900",
        },
        darkColors: {
          main: "#fffcc0",
          onContainer: "#ffdede",
          container: "#a0a242",
        },
      },
    },
    plugins: [createEventModalPlugin(), createDragAndDropPlugin()],
  });

  return (
    <div>
      <ScheduleXCalendar calendarApp={calendar} />
    </div>
  );
}

export default CalendarDisplay;
