package mononocle

object MonocleExample extends App {
  case class Street(number: Int, name: String)
  case class Address(city: String, street: Street)
  case class Company(name: String, address: Address)
  case class Employee(name: String, company: Company)

  val employee = Employee("john", Company("awesome inc", Address("london", Street(23, "high street"))))

  // Capitalize street name
  employee.copy(
    company = employee.company.copy(
      address = employee.company.address.copy(
        street = employee.company.address.street.copy(
          name = employee.company.address.street.name.capitalize
        )
      )
    )
  )

  import monocle.Lens
  import monocle.macros.GenLens

  val company: Lens[Employee, Company] = GenLens[Employee](_.company)
  val address: Lens[Company, Address]  = GenLens[Company](_.address)
  val street: Lens[Address, Street]    = GenLens[Address](_.street)
  val streetName: Lens[Street, String] = GenLens[Street](_.name)

  val capitalizeStreet = {
    // Lens[A,B] composeLens Lens[B,C] == Lens[A,C]
    val l = company.composeLens(address).composeLens(street).composeLens(streetName)
    l.modify(_.capitalize)
  }

  println(capitalizeStreet(employee))

  import monocle.function.Cons.headOption // to use headOption (an optic from Cons typeclass)

  val capitalizeStreet2 = {
    // All Lenses can be seen as Optionals
    val l = company.composeLens(address).composeLens(street).composeLens(streetName).composeOptional(headOption)
    l.modify(_.toUpper)
  }

  println(capitalizeStreet2(employee))

  import monocle.macros.syntax.lens._

  val capitalizeStreet3: Employee => Employee =
    _.lens(_.company.address.street.name).composeOptional(headOption).modify(_.toUpper)

  println(capitalizeStreet3(employee))

  // ----------------------------------------
  // ----------------------------------------
  // ----------------------------------------
  // ----------------------------------------

  case class Lecturer(firstName: String, lastName: String, salary: Int)
  case class Department(budget: Int, lecturers: List[Lecturer])
  case class University(name: String, departments: Map[String, Department])

  val uni = University(
    "oxford",
    Map(
      "Computer Science" -> Department(
        45,
        List(
          Lecturer("john", "doe", 10),
          Lecturer("robert", "johnson", 16)
        )
      ),
      "History" -> Department(
        30,
        List(
          Lecturer("arnold", "stones", 20)
        )
      )
    )
  )

  val departments = GenLens[University](_.departments)

  import monocle.function.At.at // to get at Lens
  import monocle.std.map._ // to get Map instance for At

  println(departments.composeLens(at("History")).set(None)(uni))

  val physics = Department(
    36,
    List(
      Lecturer("daniel", "jones", 12),
      Lecturer("roger", "smith", 14)
    )
  )

  println(departments.composeLens(at("Physics")).set(Some(physics))(uni))

  val lecturers = GenLens[Department](_.lecturers)
  val salary    = GenLens[Lecturer](_.salary)

  import monocle.function.all._ // to get each and other typeclass based optics such as at or headOption
  import monocle.Traversal // zooms into all elements of a container (e.g. Map Vector List)

  val allLecturers: Traversal[University, Lecturer] =
    departments.composeTraversal(each).composeLens(lecturers).composeTraversal(each)

  val increaseAllSalaries = allLecturers.composeLens(salary).modify(_ + 2)

  println(increaseAllSalaries(uni))

  val firstName = GenLens[Lecturer](_.firstName)
  val lastName  = GenLens[Lecturer](_.lastName)

  import monocle.std.string._ // to get String instance for Cons

  // annoying
  val upperCasedFirstName = allLecturers.composeLens(firstName).composeOptional(headOption).modify(_.toUpper)
  val upperCasedLasttName = allLecturers.composeLens(lastName).composeOptional(headOption).modify(_.toUpper)

  val firstAndLastNames =
    Traversal.apply2[Lecturer, String](_.firstName, _.lastName) {
      case (fn, ln, l) => l.copy(firstName = fn, lastName = ln)
    }

  val upperCasedFirstLastName =
    allLecturers.composeTraversal(firstAndLastNames).composeOptional(headOption).modify(_.toUpper)

  upperCasedFirstLastName(uni)
}
