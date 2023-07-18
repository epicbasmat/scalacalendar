/*All Rights belong to De Montfort University, 2023*/

package application

import lib.picture.Picture

import scala.collection.immutable.{ListMap, TreeMap}

object MyCalendar {
  /** THE COURSEWORK METHOD
   * **********************************************************************************
   * Produces a picture (ready for display on the output console) of the given month in
   * the calendar.  The events are filtered so that only those relevant for the given
   * month are displayed.
   ************************************************************************************
   * @param year   The calendar year. (e.g. 2023)
   * @param month  The calendar month (1 = JANUARY, etc.)
   * @param events The sequence of (all) events. It will need to be filtered to obtain
   *               those events relevant for this month and this year.
   * @return A nicely formatted calendar page that contains a view of the given
   *         month in the given year.
   */
  def displayMonth(year: Int, month: Int, events: Seq[Event]) = {

    //The calendar produced is based off of the windows 11 calendar, which is a fixed 7x6 calendar, and shows all dates before and after the month.

    /**
     * This method returns a sequence containing all relevant events within a date range, specified by the parameters.
     *
     * @param year       The year of the specific events
     * @param month      The month of the specific events
     * @param lowestDay  The lowest day of the specific events
     * @param highestDay The highest day of the specifc events
     * @param events     The total sequence of events
     * @return A list containing all events within the date range
     */
    def getRelevantEvents(year: Int, month: Int, lowestDay: Int, highestDay: Int, events: Seq[Event]): Seq[Event] = {
      events.filter(event => event.date.year == year && event.date.month == month && (event.date.day <= highestDay && event.date.day >= lowestDay))
    }

    /**
     * Returns the combined value of the time of the specified event
     *
     * @param event The event to get the time of
     * @return The combined value of the time of the event
     */
    def addTime(event: Event): Int = {
      if (event.time.minute == 0) {
        Integer.parseInt("" + event.time.hour + "00");
      } else {
        Integer.parseInt("" + event.time.hour + event.time.minute);
      }
    }

    /**
     * Returns the previous month before the last, dealing with the edge case of January.
     *
     * @param year  The year of the month
     * @param month The month to get the previous one of
     * @return A tuple of (Int, Int) => (Year, Month), which is the month before the passed month.
     */
    def getPreviousMonth(year: Int, month: Int): (Int, Int) = month match {
      case 1 => (year - 1, 12);
      case _ => (year, month - 1);
    }

    /**
     * Returns the month after the passed month, dealing with the edge case of December
     *
     * @param year  The year of the month
     * @param month The month to get the next one of
     * @return A tuple of (Int, Int) => (Year, Month), which is the month after the passed month
     */
    def getNextMonth(year: Int, month: Int): (Int, Int) = month match {
      case 12 => (year + 1, 1)
      case _ => (year, month + 1);
    }

    /**
     * Returns the combined value of the date of the specified event
     *
     * @param event The event to get the date of
     * @return The combined value of the date of the event.
     */
    def addDates(event: Event): Int = {
      event.date.year + event.date.month + event.date.day
    }

    /**
     * Compares one event to another event, returning <code>true</code> if the first date is less than the last date, else <code>false</code>. Also will compare the time if the date is the same.
     *
     * @param event1 The first event to compare
     * @param event2 The second event to compare
     * @return A boolean indicating if the first event is greater than the second event
     */
    def sortByDate(event1: Event, event2: Event): Boolean = {
      if (addDates(event1) < addDates(event2)) {
        true
      } else if (addDates(event1) == addDates(event2)) {
        if (addTime(event1) < addTime(event2)) {
          true
        } else {
          false
        }
      } else {
        false
      }
    }

    /**
     * Returns a Sequence of pictures from min to max that can contain both Picture(events) or Picture(empty).
     *
     * @param min    The minimum day to generate a picture
     * @param max    The maximum day to generate a picture
     * @param events The events to refer to. If an event exists on a day, no empty picture is created
     * @return An ordered Seq[Picture] which has both empty days and days with events.
     */
    def fillEmptyDays(min: Int, max: Int, events: Map[Int, Picture]): Seq[Picture] = {
      for (i <- min to max) yield events.getOrElse(i, Picture(i.toString))
    }

    /**
     * picturizeMonth returns a Sequence of pictures that were constructed entirely from events.
     *
     * @param filteredEvents The events to turn into Pictures.
     * @return A Map[Int, Picture] which maps the days that have events, to their associated Pictures.
     */
    def picturizeEvents(filteredEvents: Seq[Event]): Map[Int, Picture] = {

      //Filter and then sort by date and time to create a chronological sequence of events, the day sorting gets lost with the grouping, but the time sorting remains.
      val sortedEvents: Seq[Event] = filteredEvents.sortWith(sortByDate)

      //Then group them by the days
      val groupedByDays: Map[Int, Seq[Event]] = sortedEvents.groupBy(_.date.day)

      //Then generate the picture's description
      val eventPicturesIndividual: Map[Int, Seq[Picture]] = groupedByDays.map((day, event) => (day, event.map(x => Picture(x.desc))))

      //For each map of K -> V, assemble all value pictures into one picture
      val eventPicturesAssembled: Map[Int, Picture] = eventPicturesIndividual.map((day, eventSeq) => (day, eventSeq.reduceLeft(_ ^ _)))

      //Then add the day on top
      val events: Map[Int, Picture] = eventPicturesAssembled.map((day, picture) => (day, Picture(day.toString) ^ picture))
      //println(events)
      events
    }

    /**
     * This method generates a Sequence of Pictures from a range within a month, from the lowest day to the highest day. The picture sequence is full. i.e it contains events within a day, and also empty events.
     *
     * @param year               The year to process
     * @param month              The month to process
     * @param lowestDay          The lowest day within the month to include
     * @param highestDay         The highest day within the month to include
     * @param getEventsFromMonth <code> true </code> if the returning sequence should include events, <code> false </code> if the returning sequence should not have any events.
     * @param events             The total sequence of Events to filter through.
     * @return a Seq[Picture] of the range of dates in the designated month.
     */
    def generateMonthsPictures(year: Int, month: Int, lowestDay: Int, highestDay: Int, getEventsFromMonth: Boolean, events: Seq[Event]): Seq[Picture] = {
      //Generate a range of pictures within a month. If we want the events, we call picturizeEvent with the parameters to return a sequence of events in picture form, and then thats filled with the empty days.
      //If the boolean is false, then fillEmptyDays gets an empty map, so returns a full sequence of empty pictures.
      val monthPictures: Seq[Picture] = fillEmptyDays(lowestDay, highestDay, if (getEventsFromMonth) picturizeEvents(getRelevantEvents(year, month, lowestDay, highestDay, events)) else Map.empty)
      monthPictures
    }

    /*
     * This variable, and the logic around it, exists because I wasn't sure about how to handle this.
     * Some calendars will show the events surrounding a month, such as the windows 11 calendar, whereas the coursework specifies a months range
     * So, to handle this, generateSisterMonthEvents exists. Changing it to true will fill the surrounding empty space of a month with any events that it can find.
     * By default, it's disabled. Just to be on the safe side.
     */
    val generateSisterMonthEvents: Boolean = false

    //Generate this months pictures
    val daysInCurrentMonth:Int = daysInMonth(year, month)
    val thisMonthsPictures: Seq[Picture] = generateMonthsPictures(year, month, 1, daysInCurrentMonth, true, events)

    //Get where the month needs to start, and from this we also get the amount of spaces, before the month begins, to fill
    val getFirstDayOfWeek: Int = getDayOfWeek(year, month, 1)
    val lastMonth:(Int, Int) = getPreviousMonth(year, month)
    val daysInLastMonth: Int = daysInMonth(lastMonth._1, lastMonth._2)
    val startingDayToFill:Int = daysInLastMonth - getFirstDayOfWeek + 1

    val lastMonthsPictures: Seq[Picture] = generateMonthsPictures(lastMonth._1, lastMonth._2, startingDayToFill, daysInLastMonth, generateSisterMonthEvents, events)

    //Get where the month ends, and from this get the amount of remaining space
    val daysInWeek: Int = 7
    val weeksInCalendar: Int = 6
    val nextMonth: (Int, Int) = getNextMonth(year, month)
    val spacesAfterMonth: Int = daysInWeek * weeksInCalendar - daysInCurrentMonth - getFirstDayOfWeek

    val nextMonthsPictures: Seq[Picture] = generateMonthsPictures(nextMonth._1, nextMonth._2, 1, spacesAfterMonth, generateSisterMonthEvents, events)

    //Begin to generate the full calendar
    val calendarDataOnly: Seq[Picture] = lastMonthsPictures ++ thisMonthsPictures ++ nextMonthsPictures;

    //Normalize the depth to make it look more normal
    val calendarBulked: Seq[Picture] = calendarDataOnly.normaliseDepth()

    val calendarWithHeader: Seq[Picture] = namesOfDays ++ calendarBulked

    val calendarNoBorders: Seq[Seq[Picture]] = calendarWithHeader.grouped(7).toSeq
    //println(calendarNoBorders)

    //Grab month name
    val name: Picture = nameOfMonth.getOrElse(month, Picture("Failure"))

    //The MM/YY that displays at the top of the calendar.
    val dateHeader: Picture = name + Picture(" " + year.toString);

    val calendar: Picture = dateHeader ^ calendarNoBorders.formatAsTable()

    calendar
  }
}
