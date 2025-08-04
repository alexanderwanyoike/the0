'use client';
import React from 'react';
import { NavigationMenu } from '@/components/landing-page/navigation-menu';

export default function TermsPage() {
  return (
    <div className="min-h-screen bg-background">
      <NavigationMenu />

      <div className="container max-w-4xl py-16">
        {/* Header */}
        <div className="mb-8">
          <h1 className="text-4xl font-bold mb-4">Terms of Service</h1>
          <p className="text-muted-foreground">Last updated: April 24, 2025</p>
        </div>

        {/* Agreement Section */}
        <div className="prose prose-gray dark:prose-invert max-w-none">
          <section className="mb-8">
            <h2 className="text-2xl font-bold mb-4">
              Agreement to Our Legal Terms
            </h2>
            <p className="text-muted-foreground">
              We are Alpha Neuron LTD (&#39;Company&#39;, &#39;we&#39;,
              &#39;us&#39;, or &#39;our&#39;), a company registered in the
              United Kingdom at 406 4 Box Works, Manchester, Manchester M154NU.
            </p>
            <p className="text-muted-foreground mt-4">
              We operate the website https://the0.dev (the &#39;Site&#39;), as
              well as any other related products and services that refer or link
              to these legal terms (the &#39;Legal Terms&#39;) (collectively,
              the &#39;Services&#39;).
            </p>
            <p className="text-muted-foreground mt-4">
              You can contact us by phone at (+44)07376195527, email at
              legal@alphaneuron.net, or by mail to 406 4 Box Works, Manchester,
              Manchester M154NU, United Kingdom.
            </p>
          </section>

          <section className="mb-8">
            <h2 className="text-2xl font-bold mb-4">Our Services</h2>
            <p className="text-muted-foreground">
              The Services are intended for users who are at least 18 years old.
              Persons under the age of 18 are not permitted to use or register
              for the Services.
            </p>
          </section>

          <section className="mb-8">
            <h2 className="text-2xl font-bold mb-4">Pricing and Payments</h2>
            <p className="text-muted-foreground">
              We accept the following forms of payment:
            </p>
            <ul className="list-disc list-inside text-muted-foreground mt-4">
              <li>Visa</li>
              <li>Mastercard</li>
              <li>American Express</li>
              <li>Discover</li>
            </ul>
            <p className="text-muted-foreground mt-4">
              You agree to provide current, complete, and accurate purchase and
              account information for all purchases made via the Services. You
              further agree to promptly update account and payment information,
              including email address, payment method, and payment card
              expiration date, so that we can complete your transactions and
              contact you as needed.
            </p>
          </section>

          <section className="mb-8">
            <h2 className="text-2xl font-bold mb-4">Subscriptions</h2>
            <p className="text-muted-foreground">
              Your subscription will continue and automatically renew unless
              cancelled. You consent to our charging your payment method on a
              recurring basis without requiring your prior approval for each
              recurring charge, until such time as you cancel the applicable
              order. The length of your billing cycle is monthly.
            </p>
            <p className="text-muted-foreground mt-4">
              You can cancel your subscription at any time by logging into your
              account. Your cancellation will take effect at the end of the
              current paid term.
            </p>
          </section>

          <section className="mb-8">
            <h2 className="text-2xl font-bold mb-4">
              Intellectual Property Rights
            </h2>
            <p className="text-muted-foreground">
              We are the owner or the licensee of all intellectual property
              rights in our Services, including all source code, databases,
              functionality, software, website designs, audio, video, text,
              photographs, and graphics in the Services (collectively, the
              &#39;Content&#39;), as well as the trademarks, service marks, and
              logos contained therein (the &#39;Marks&#39;).
            </p>
          </section>

          <section className="mb-8">
            <h2 className="text-2xl font-bold mb-4">Contact Information</h2>
            <div className="space-y-2 text-muted-foreground">
              <p>Alpha Neuron LTD</p>
              <p>406 4 Box Works</p>
              <p>Manchester, Manchester M154NU</p>
              <p>United Kingdom</p>
              <p>Phone: (+44)07376195527</p>
              <p>Email: legal@alphaneuron.net</p>
            </div>
          </section>
        </div>
      </div>
    </div>
  );
}
