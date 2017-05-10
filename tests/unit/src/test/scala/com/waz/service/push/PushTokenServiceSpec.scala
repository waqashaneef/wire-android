/*
 * Wire
 * Copyright (C) 2016 Wire Swiss GmbH
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package com.waz.service.push

import com.waz.content.AccountsStorage
import com.waz.content.GlobalPreferences.PushEnabledKey
import com.waz.content.Preferences.Preference
import com.waz.model._
import com.waz.service.ZmsLifecycle
import com.waz.specs.AndroidFreeSpec
import com.waz.sync.SyncServiceHandle
import com.waz.testutils.TestGlobalPreferences
import com.waz.threading.Threading
import com.waz.utils.events.{Signal, SourceSignal}
import com.waz.utils.returning
import com.waz.utils.wrappers.GoogleApi

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class PushTokenServiceSpec extends AndroidFreeSpec {
  
  val google    = mock[GoogleApi]
  val lifecycle = mock[ZmsLifecycle]
  val accounts  = mock[AccountsStorage]
  val sync      = mock[SyncServiceHandle]
  val prefs     = new TestGlobalPreferences()
  val accountId = AccountId()

  var pushEnabled: Preference[Boolean] = _

  var googlePlayAvailable: SourceSignal[Boolean] = _
  var lifecycleActive: SourceSignal[Boolean] = _
  var accountSignal: SourceSignal[AccountData] = _

  val defaultDuration = 5.seconds

  before {

    prefs.reset()

    pushEnabled = prefs.preference(PushEnabledKey)

    googlePlayAvailable = Signal(false)
    lifecycleActive = Signal(false)
    accountSignal = Signal[AccountData]()
  }

  feature("Token generation") {
    scenario("Fetches token on init if GCM available") {
      val token = "token"
      (google.getPushToken _).expects().returning(token)
      val service = initTokenService()

      pushEnabled := true
      googlePlayAvailable ! true
      Await.ready(service.currentTokenPref.signal.filter(_.contains(token)).head, defaultDuration)
    }

    scenario("Remove Push Token event should create new token and delete all previous tokens") {

      val oldToken = "oldToken"
      val newToken = "newToken"
      var calls = 0
      (google.getPushToken _).expects().anyNumberOfTimes().onCall { () =>
        calls += 1
        calls match {
          case 1 => oldToken
          case 2 => newToken
          case _ => fail("Too many calls to getPushToken!")
        }
      }

      //This needs to be called
      (google.deleteAllPushTokens _).expects().once()

      val service = initTokenService()

      pushEnabled := true
      googlePlayAvailable ! true
      //wait for first token to be set
      Await.ready(service.currentTokenPref.signal.filter(_.contains(oldToken)).head, defaultDuration)
      //delete first token in response to BE event
      service.eventProcessingStage(RConvId(), Vector(GcmTokenRemoveEvent(oldToken, "sender", Some("client"))))
      //new token should be set
      Await.ready(service.currentTokenPref.signal.filter(_.contains(newToken)).head, defaultDuration)
    }
  }

  feature("Token registration") {
    scenario("If current user does not have matching registeredPush token, register the user with our BE") {

      accountSignal ! AccountData(accountId, None, "", None, None, Some("oldToken"))
      pushEnabled := true
      googlePlayAvailable ! true

      val token = "token"
      (google.getPushToken _).expects().anyNumberOfTimes().returning(token)

      lazy val service = initTokenService()

      (sync.registerPush _).expects().anyNumberOfTimes().onCall { () =>
        Future {
          service.onTokenRegistered()
          SyncId()
        } (Threading.Background)
      }

      Await.result(service.currentTokenPref.signal.filter(_.contains(token)).head, defaultDuration)
      Await.result(accountSignal.filter(_.registeredPush.contains(token)).head, defaultDuration)
    }

    scenario("Instance Id token refresh should trigger re-registration for current user") {
      accountSignal ! AccountData(accountId, None, "", None, None, Some("token"))
      pushEnabled := true
      googlePlayAvailable ! true

      val token = "token"
      (google.getPushToken _).expects().anyNumberOfTimes().returning(token)

      lazy val service = initTokenService()

      (sync.registerPush _).expects().anyNumberOfTimes().onCall { () =>
        Future {
          service.onTokenRegistered()
          SyncId()
        } (Threading.Background)
      }

      Await.ready(service.currentTokenPref.signal.filter(_.contains(token)).head, defaultDuration)
      Await.ready(accountSignal.filter(_.registeredPush.contains(token)).head, defaultDuration)

      val newToken = "newToken"
      service.onTokenRefresh ! Some(newToken) //InstanceIDService triggers new token

      Await.ready(service.currentTokenPref.signal.filter(_.contains(newToken)).head, defaultDuration)
      Await.ready(accountSignal.filter(_.registeredPush.contains(newToken)).head, defaultDuration)

    }

    scenario("Non-matching user/global tokens should delete old user token on BE.") {
      fail()
    }
  }

  feature("Push active") {

    scenario("Push should be active if enabled and inactive if not") {
      val token = "token"
      (google.getPushToken _).expects().anyNumberOfTimes().returning(token)
      val service = initTokenService()

      pushEnabled := true //set active
      googlePlayAvailable ! true

      Await.ready(service.pushActive.filter(_ == true).head, defaultDuration)

      pushEnabled := false //set inactive
      Await.ready(service.pushActive.filter(_ == false).head, defaultDuration)
    }

    scenario("Push should be active if in background and inactive if not") {
      val token = "token"
      (google.getPushToken _).expects().anyNumberOfTimes().returning(token)
      val service = initTokenService()

      pushEnabled := true
      googlePlayAvailable ! true
      lifecycleActive ! true //websocket should be open

      Await.ready(service.pushActive.filter(_ == false).head, defaultDuration)

      lifecycleActive ! false //websocket should be off - use push again
      Await.ready(service.pushActive.filter(_ == true).head, defaultDuration)
    }

    scenario("Push should be inactive if play services are unavailable") {
      val token = "token"
      (google.getPushToken _).expects().anyNumberOfTimes().returning(token)
      val service = initTokenService()

      pushEnabled := true
      googlePlayAvailable ! true

      Await.ready(service.pushActive.filter(_ == true).head, defaultDuration)

      googlePlayAvailable ! false //Set true and then false again to cause the signal to change. Maybe the user disables play services?
      Await.ready(service.pushActive.filter(_ == false).head, defaultDuration)
    }
  }

  def initTokenService(google:    GoogleApi         = google,
                       lifecycle: ZmsLifecycle      = lifecycle,
                       accountId: AccountId         = accountId,
                       accounts:  AccountsStorage   = accounts,
                       sync:      SyncServiceHandle = sync) = {

    (google.isGooglePlayServicesAvailable _).expects().anyNumberOfTimes().returning(googlePlayAvailable)
    (accounts.signal _).expects(*).anyNumberOfTimes().returning(accountSignal)
    (lifecycle.active _).expects().anyNumberOfTimes().returning(lifecycleActive)
    (accounts.update _).expects(accountId, *).anyNumberOfTimes().onCall { (_, f) =>
      Future {
        returning(accountSignal.currentValue("").fold(Option.empty[(AccountData, AccountData)])(p => Some((p, f(p))))) {
          case Some((_, updated)) => accountSignal ! updated
          case _ =>
        }
      }(Threading.Background)
    }

    new PushTokenService(google, prefs, lifecycle, accountId, accounts, sync)
  }
}
