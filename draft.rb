# Backfill legacy tracked events to the new AggregateEvent table.
# Note that AggregateEvents are rolled up TrackableEvents.

# There is an integration-y spec in spec/tasks/aggregate_events_rake_spec.rb
module AggregateEventHelper

  VALID_T_FRAMES = %w(hour day week)

  BACKFILL_EVENT_TARGETS = %w(profile_views searches profiles_contacted listings notes exports info_requests)

  module User
    class << self

      def backfill(user, t_frame:, t_range:)
        return if ( user.last_active_at.nil? || user.last_active_at < t_range.first )

        BACKFILL_EVENT_TARGETS.each do |event_target|
          events_hash = {}

          created_at = user.created_at.send("beginning_of_#{t_frame}")
          time_window_started_at = [ created_at, t_range.first ].max
          time_window = time_window_started_at..t_range.last

          Rails.logger.info "Linh Calculating aggregate event for user #{user.id} for #{t_frame}ly #{event_target} within time_window #{time_window_started_at} and #{t_range.last}"

          events = pull_related_events_from_database(user, event_target, time_window)

          next unless events.present?

          events.each do |event|
            Rails.logger.debug "working on #{event_target} event id ##{event.id}"
            period_range = calculate_time_slot(event, event_target, t_frame)
            events_hash[period_range] ||= 0
            events_hash[period_range] += calculate_increment_count(event, event_target)
          end

          events_hash.each do |period_range, event_count|
            Rails.logger.info "Linh generating aggregate event for #{event_target} during #{period_range} with #{event_count}"
            parent.generate_aggregate_events(
              object_id: user.id,
              class_name: 'User',
              timeframe_type: t_frame,
              event_target: event_target,
              event_count: event_count,
              period_range: period_range
            )
          end
        end

        # Add this temporarily to also fill in AggregateEventTmp based on UserEvent
        created_at = user.created_at.send("beginning_of_#{t_frame}")
        time_window_started_at = [ created_at, t_range.first ].max
        time_window = time_window_started_at..t_range.last

        period_started_at = t_range.last.send("beginning_of_#{t_frame}")
        period_ended_at = t_range.last.send("end_of_#{t_frame}")
        period_range = (period_started_at..period_ended_at)

        user_events = UserEvent.where(user_id: user.id, event_at: period_range)
        user_event_aggregate_service = UserEventAggregatorService.new(user_events)

        BACKFILL_EVENT_TARGETS.each do |event_target|
          events = user_event_aggregate_service.public_send("#{event_target}")
          next unless events.present?

          aggregate_event_tmp = AggregateEventTmp.where({
            :aggregate_id => user.id,
            :aggregate_type => 'User',
            :product => 'search',
            :event_type => 'backfill',
            :event_target => event_target,
            :timeframe_type => t_frame,
            :period_started_at => period_range.first,
            :period_ended_at => period_range.last,
          }).first_or_initialize

          aggregate_event_tmp.event_count = events.count

          unless aggregate_event_tmp.save
            Rails.logger.debug "Error is #{aggregate_event_tmp.errors.to_h}"
          end
        end

      end

      def calculate_increment_count(event, event_target)
        if event_target == "profile_views"
          # select count(*) from profile_actions where action_id = 1 and count is null;
          # => 137180
          event.count || 1
        elsif event_target == "profiles_contacted"
          1
        elsif event_target == "exports"
          event.credits_spent
        else
          1
        end
      end

      def calculate_time_slot(event, event_target, t_frame)
        if event_target == "profile_views" || event_target == "profiles_contacted"
          related_date = event.updated_at
        else
          related_date = event.created_at
        end

        period_started_at = related_date.send("beginning_of_#{t_frame}")
        period_ended_at = related_date.send("end_of_#{t_frame}")
        (period_started_at..period_ended_at)
      end

      def pull_related_events_from_database(user, event_target, time_window)
        case event_target
        when "profile_views"
          affiliation = user.current_affiliation
          return unless affiliation

          ProfileAction.og_unmerged.where(
            action_id:      1,
            affiliation_id: affiliation.id,
            merged: nil,
            updated_at: time_window
          )

        when "searches"
          Searching.where(
            user_id: user.id,
            created_at: time_window
          )

        when "profiles_contacted"
          affiliation = user.current_affiliation
          return unless affiliation

          ProfileAction.og_unmerged.where(
            action_id:      2,
            affiliation_id: affiliation.id,
            merged: nil,
            updated_at: time_window
          )

        when "listings"
          Listing.unmerged.where(
            user_id: user.id,
            created_at: time_window
          )

        when "notes"
          Note.og_unmerged.where(
            user_id: user.id,
            created_at: time_window
          )

        when "exports"
          AppliedCredit.og_unmerged.where(
            user_id: user.id,
            spent_on: ['Pdf Export', 'Profile Export'],
            created_at: time_window,
          )

        when "info_requests"
          InfoRequest.where(
            requester_id: user.id,
            created_at: time_window,
          )
        end
      end
    end
  end

  # Org backfill globs together AggregateEvents of each org User.
  module Organization
    class << self

      def backfill(org, t_frame:, t_range:)
        BACKFILL_EVENT_TARGETS.each do |event_target|
          Rails.logger.debug "Starting to calculate aggregate event for org #{org.id} for #{t_frame}ly #{event_target}"

          user_ids = org.users.pluck(:id)
          events_hash = {}

          org_created_at = org.created_at.send("beginning_of_#{t_frame}")
          time_window_started_at = [ org_created_at, t_range.first ].max
          time_window = time_window_started_at..t_range.last

          events = \
            AggregateEvent.where(
              aggregate_type: 'User',
              aggregate_id: user_ids,
              timeframe_type: t_frame,
              event_type: 'backfill',
              event_target: event_target,
              period_started_at: time_window,
            )

          next unless events.present?

          events.each do |event|
            Rails.logger.debug "working on #{event_target} event id ##{event.id}"
            period_range = (event.period_started_at..event.period_ended_at)
            events_hash[period_range] ||= 0
            events_hash[period_range] += event.event_count
          end

          events_hash.each do |period_range, event_count|
            Rails.logger.debug "Linh generating aggregate event for #{event_target} during #{period_range} with #{event_count}"
            parent.generate_aggregate_events(
              object_id: org.id,
              class_name: 'Organization',
              timeframe_type: t_frame,
              event_target: event_target,
              event_count: event_count,
              period_range: period_range
            )
          end
        end
      end

      def destroy_events(org, t_range:)
        AggregateEvent\
          .where(
            aggregate_type: 'Organization',
            aggregate_id: org_id
          )\
          .where( period_started_at: t_range )\
          .destroy_all

        user_ids = org.users.pluck(:id)
        AggregateEvent\
          .where(
            aggregate_type: 'Organization',
            aggregate_id: user_ids
          )\
          .where( period_started_at: t_range )\
          .destroy_all
      end
    end
  end

  # Helpers
  class << self
    def generate_aggregate_events(object_id:, class_name:, timeframe_type:, event_target:, event_count:, period_range:)
      aggregate_event = AggregateEvent.where({
        :aggregate_id => object_id,
        :aggregate_type => class_name,
        :product => 'search',
        :event_type => 'backfill',
        :event_target => event_target,
        :timeframe_type => timeframe_type,
        :period_started_at => period_range.first,
        :period_ended_at => period_range.last,
      }).first_or_initialize

      aggregate_event.event_count = event_count

      unless aggregate_event.save
        Rails.logger.debug "Failing to create aggregate event for #{klass_name} id #{object.id}, for period  #{period_range}, of event #{event_target}"
        Rails.logger.debug "Error is #{aggregate_event.errors.to_h}"
      end
    end

    def valid_t_frame?(t_frame)
      VALID_T_FRAMES.include?(t_frame)
    end
  end
end
