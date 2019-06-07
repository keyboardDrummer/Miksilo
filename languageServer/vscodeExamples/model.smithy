namespace example.weather

service Weather {
  version: "2006-03-01",
  resources: [City],
  operations: [GetCurrentTime]
}

resource City {
  identifiers: { cityId: CityId },
  read: GetCity,
  list: ListCities,
  resources: [Forecast],
}

resource Forecast {
  type: resource,
  identifiers: { cityId: CityId },
  read: GetForecast,
}

// "pattern" is a trait.
@pattern("^[A-Za-z0-9 ]+$")
string CityId

@readonly
operation GetCity(GetCityInput) -> GetCityOutput errors [NoSuchResource]

structure GetCityInput {
  // "cityId" provides the identifier for the resource and
  // has to be marked as required.
  @required
  cityId: CityId
}

structure GetCityOutput {
  // "required" is used on output to indicate if the service
  // will always provide a value for the member.
  @required
  name: smithy.api#String,

  @required
  coordinates: CityCoordinates,
}

// This structure is nested within GetCityOutput.
structure CityCoordinates {
  @required
  latitude: smithy.api#Float,

  @required
  longitude: smithy.api#Float,
}

// "error" is a trait that is used to specialize
// a structure as an error.
@error(client)
structure NoSuchResource {
  @required
  resourceType: smithy.api#String
}

// The paginated trait indicates that the operation may
// return truncated results.
@readonly
@paginated(inputToken: nextToken, outputToken: nextToken,
          pageSize: pageSize, items: items)
operation ListCities(ListCitiesInput) -> ListCitiesOutput

structure ListCitiesInput {
  nextToken: smithy.api#String,
  pageSize: smithy.api#Integer
}

string nextToken // Introduce some garbage into the scope

structure ListCitiesOutput {
  nextToken: ListCitiesInput$nextToken,

  @required
  items: CitySummaries,
}

// CitySummaries is a list of CitySummary structures.
list CitySummaries {
  member: CitySummary
}

// CitySummary contains a reference to a City.
@references(city: { resource: City, service: Weather })
structure CitySummary {
  @required
  cityId: CityId,

  @required
  name: smithy.api#String,
}

@readonly
operation GetCurrentTime() -> GetCurrentTimeOutput

structure GetCurrentTimeOutput {
  @required
  time: smithy.api#Timestamp
}