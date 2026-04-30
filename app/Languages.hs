module Languages where

import Scotty (AppError (..))

newtype ShowAppError = ShowAppError AppError
instance Show ShowAppError where
  show (ShowAppError PasswordMismatch) = "Le password non corrispondono"
  show (ShowAppError WeakPassword) = "La password deve essere lunga almeno 8 caratteri"
  show (ShowAppError InvalidEmail) = "L'indirizzo email non è valido"
  show (ShowAppError InvalidFullname) = "Il nome completo non può essere vuoto"
  show (ShowAppError UserAlreadyExists) = "Un utente con questa email esiste già"
  show (ShowAppError HashError) = "Errore generico"
  show (ShowAppError InvalidCredentials) = "Email o password non validi"
  show (ShowAppError (GingerError _)) = "Si è verificato un errore durante il rendering della pagina"

