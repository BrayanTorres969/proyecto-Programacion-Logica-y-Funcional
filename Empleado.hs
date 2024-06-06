module Empleado (
    Empleado,
    crearEmpleado,
    buscarEmpleado,
    actualizarEmpleado,
    eliminarEmpleado,
    listarEmpleados
) where

-- Definimos el tipo de datos Empleado
data Empleado = Empleado
  { empleadoDNI :: Int
  , nombres     :: String
  , apellidos   :: String
  , puesto      :: String
  , salario     :: Double
  } deriving (Show, Eq)

-- Crear un nuevo empleado
crearEmpleado :: Int -> String -> String -> String -> Double -> [Empleado] -> [Empleado]
crearEmpleado dni nombres apellidos puesto salario empleados = empleados ++ [Empleado dni nombres apellidos puesto salario]

-- Buscar un empleado por DNI
buscarEmpleado :: Int -> [Empleado] -> Maybe Empleado
buscarEmpleado dni empleados = case filter (\e -> empleadoDNI e == dni) empleados of
    []    -> Nothing
    (x:_) -> Just x

-- Actualizar un empleado existente
actualizarEmpleado :: Int -> String -> String -> String -> Double -> [Empleado] -> [Empleado]
actualizarEmpleado dni nuevoNombres nuevoApellidos nuevoPuesto nuevoSalario empleados = 
    map (\e -> if empleadoDNI e == dni then e { nombres = nuevoNombres, apellidos = nuevoApellidos, puesto = nuevoPuesto, salario = nuevoSalario } else e) empleados

-- Eliminar un empleado por DNI
eliminarEmpleado :: Int -> [Empleado] -> [Empleado]
eliminarEmpleado dni empleados = filter (\e -> empleadoDNI e /= dni) empleados

-- Listar todos los empleados
listarEmpleados :: [Empleado] -> [Empleado]
listarEmpleados = id
