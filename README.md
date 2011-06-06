# Digestive functors snap backend to work with heist frontend



## State of this library

Right now, this is a combined frontend (for Heist) and backend (for Snap). This is because, due to a limitation in digestive-functors that prevents us from requesting a piece of the environment in any way other than using a field name like "prefix-fval[1]" or in the case of subforms "prefix-fval[1.1]", there is a lot of muckery between frontend and backend going on (like, the backend gives back the whole environment, and the frontend finds the correct input. Or, numeric Ids are used until the end, in an overridden version of runForm, where the mapping to names is done). In the future, ideally digestive-functors can expand to be able to accommodate this kind of behavior (ie, specifying fields by non-integer ids), at which point this could turn into purely a Heist frontend (what it started as) and could plug into the existing Snap backend. There are a couple ways this could happen:

One way to alleviate this would be to make the FormId that is passed to the Environment (which is provided by the backend) be polymorphic, restricted to, for example, something that can be turned into a string. The downsides of that is further complicating the code by being even more abstract, and, having to thread that type throughout.

Either way, this library should be seen as an early attempt to bring Digestive Functors (which are an amazingly powerful thing) to Heist, and NOT thought to be an ideal solution. (or even close to ideal)

## The good

With that said, the reason that this exists at all is that from the perspective of client code, this is complete. So if you want to write code that uses Digestive-Functors, Heist and Snap, you should be able to do that right now, and when the wiring that ties those together is fixed up, you shouldn't have to change anything except some import statements. (ideally).

## Splices provided

For each named input, these splices are created: "name-value" (just text of the value) and "name-error" which is a splice that renders subsplices with text assigned to the tag "error". ie: <name-errors><error/></name-errors>, since there is a list of errors provided. Future additions could allow that to be presented as text as well, concatenated with spaces or commas, in case someone wanted to use it as an attribute.

## Usage (bare and somewhat incomplete, also strange to print out passwords, but...)
    import Text.Digestive.Types
    import Text.Digestive.Snap.Heist
    import Text.Digestive.Validate
    import Text.Digestive.Heist
    import Text.Templating.Heist
    
    import Application
    
    data NewPassword = NewPassword String String String deriving (Eq,Show)
    
    passwordForm :: SnapForm Application Text HeistView NewPassword
    passwordForm = NewPassword
        <$> input "current" Nothing  `validate` checkPassword <++ errors
        <*> input "new"     Nothing  `validate` nonEmpty      <++ errors
        <*> input "confirm" Nothing  `validate` nonEmpty      <++ errors
        
    changePasswordH = do r <- eitherSnapForm passwordForm "change-password-form"
                         case r of
                             Left splices' -> do
                               heistLocal (bindSplices splices') $ render "profile/usersettings/password"
                             Right password' -> do
                               render "profile/usersettings/password"

    <h3>Change Password</h3>
    <form-async action="/settings/password">
      <table>
        <tr><td class="label"><label for="current">Current:</label></td> <td><input name="current" type="password" />
          Value: <current-value/>
          </td></tr>
        <tr><td class="label"><label for="new">New:</label></td> <td><input name="new" type="password" />
          Value: <new-value/>
          Errors: <new-errors><error/></new-errors></td></tr>
        <tr><td class="label"><label for="confirm">Confirm:</label></td> <td><input name="confirm" type="password" />
          Value: <confirm-value/>
          Errors: <confirm-errors><error/></confirm-errors>
          <button type="submit" title=""/></td></tr>
      </table>
    </form-async>

