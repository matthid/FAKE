# Sending Notifications to a Slack Webhook

In this article you will learn how to create a [Slack](https://slack.com) webhook integration and send a notification to it. This article assumes that you already have a Slack team setup.

**Note:  This documentation is for FAKE 5. The old documentation can be found [here](legacy-slacknotification.html)! **

[API-Reference](apidocs/fake-api-slack.html)

## Adding a Webhook Integration to a Channel

Follow the [instructions](https://my.slack.com/services/new/incoming-webhook/) for setting up an incoming webhook integration. When finished, you should have a Webhook URL that looks like "https://hooks.slack.com/services/some/random/text".

## Sending a Notification to the Webhook

    open Fake.Api

    // The webhook URL from the integration you set up
    let webhookUrl = "https://hooks.slack.com/services/some/random/text"
	
    Slack.SendNotification webhookUrl (fun p ->
        {p with
            Text = "My Slack Notification!\n<https://google.com|Click Here>!"
            Channel = "@SomeoneImportant"
            IconEmoji = ":ghost:"
            Attachments = [| 
                {Slack.NotificationAttachmentDefaults with
                    Fallback = "Attachment Plain"
                    Text = "Attachment Rich"
                    Pretext = "Attachment Pretext"
                    Color = "danger"
                    Fields = [|
                        {Slack.NotificationAttachmentFieldDefaults with
                            Title = "Field Title 1"
                            Value = "Field Value 2"}
                        {Slack.NotificationAttachmentFieldDefaults with
                            Title = "Field Title 1"
                            Value = "Field Value 2"}|]
                }
                {Slack.NotificationAttachmentDefaults with
                    Fallback = "Attachment 2 Plain"
                    Text = "Attachment 2 Rich"
                    Pretext = "Attachment 2 Pretext"
                    Color = "#FFCCDD"
                }|]
        })
    |> printfn "Result: %s"

The result should look something like this:

![alt text](pics/slacknotification/slacknotification.png "Slack Notification Result")

For additional information on the parameters, check out Slack's [Webhook Documentation](https://api.slack.com/incoming-webhooks)
